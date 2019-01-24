{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Network.IPFS.Git.RemoteHelper
    ( ProcessError
    , renderProcessError

    , processCommand
    )
where

import           Control.Concurrent.MVar (modifyMVar, newMVar, withMVar)
import           Control.Exception.Safe
                 ( MonadCatch
                 , SomeException
                 , catchAny
                 , tryAny
                 )
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as L
import           Data.Foldable (toList)
import           Data.IORef (atomicModifyIORef')
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import           Data.Traversable (for)
import           GHC.Stack (HasCallStack)
import           System.FilePath (joinPath, splitDirectories)

import           Data.IPLD.CID (CID, cidFromText, cidToText)

import qualified Data.Git.Monad as Git
import qualified Data.Git.Ref as Git
import qualified Data.Git.Revision as Git
import qualified Data.Git.Storage as Git
import qualified Data.Git.Storage.Loose as Git
import qualified Data.Git.Storage.Object as Git

import           Network.IPFS.Git.RemoteHelper.Client
import           Network.IPFS.Git.RemoteHelper.Command
import           Network.IPFS.Git.RemoteHelper.Format
import           Network.IPFS.Git.RemoteHelper.Internal
import           Network.IPFS.Git.RemoteHelper.Trans

data ProcessError
    = GitError        SomeException
    | IPFSError       ClientError
    | CidError        String
    | UnknownLocalRef Text
    deriving Show

instance DisplayError ProcessError where
    displayError = renderProcessError

renderProcessError :: ProcessError -> Text
renderProcessError = \case
    GitError  e       -> fmt ("Error accessing git repo: " % shown) e
    IPFSError e       -> renderClientError e
    CidError  e       -> fmt ("Cid conversion error: " % fstr) e
    UnknownLocalRef r -> fmt ("Ref not found locally: " % ftxt) r

processCommand :: Command -> RemoteHelper ProcessError CommandResult
processCommand Capabilities =
    pure $ CapabilitiesResult ["push", "fetch", "option"]

processCommand (Option name value) = fmap OptionResult $
    case name of
        "verbosity" -> case Text.decimal value of
            Left  e     -> pure $ OptionErr (fmt ("Invalid verbosity: " % fstr) e)
            Right (n,_) -> do
                ref <- asks envVerbosity
                liftIO . atomicModifyIORef' ref $ const (n, ())
                pure OptionOk

        "dry-run" -> do
            ref <- asks envDryRun
            let update v = liftIO . atomicModifyIORef' ref $ const (v,())
            case value of
                "true"  -> OptionOk <$ update True
                "false" -> OptionOk <$ update False
                x       -> pure $ OptionErr (fmt ("Invalid value for dry-run: " % ftxt) x)

        _ -> pure OptionUnsupported

processCommand List = fmap ListResult $ do
    root  <- asks envIpfsRoot
    paths <- ipfs $ listPaths (cidToText root) 0

    let refpath = joinPath . drop 1 . dropWhile (== "/") . splitDirectories . refPathPath
    let name    = Text.pack . refpath

    for paths $ \path ->
        case refPathType path of
            RefPathHead ->
                case hexShaFromCidText (refPathHash path) of
                    Left  e   -> throwRH $ CidError e
                    Right sha -> pure $ ListRef (Just sha) (name path) []

            RefPathRef -> do
                dest <- ipfs $ getRef (refpath path)
                pure $ ListRef (("@" <>) <$> dest) (name path) []

processCommand ListForPush = fmap ListForPushResult $ do
    root     <- asks envIpfsRoot
    branches <- do
        branches <- git Git.branchList
        pure . map (fmt $ "refs/heads/" % frefName) $ toList branches
    logDebug $ fmt ("list for-push: branches: " % shown) branches

    remoteRefs <- do
        cids <-
            forConcurrently clientMaxConns branches $ \branch ->
                ipfs (resolvePath (cidToText root <> "/" <> branch))

        for (catMaybes cids) $
            liftEitherRH . first CidError . hexShaFromCidText

    logDebug $ "list for-push: remoteRefs: " <> Text.pack (show remoteRefs)

    pure . map (\(ref, branch) -> ListRef (Just ref) branch [])
         . flip zip branches
         $ case remoteRefs of
               [] -> repeat "0000000000000000000000000000000000000000"
               xs -> xs

processCommand (Push force localRef remoteRef) =
    let
        err = PushResult . PushErr remoteRef
        ok  = PushResult . PushOk  remoteRef
     in
        fmap ok (processPush force localRef remoteRef)
            --`catchRH`  (pure . err . renderProcessError)
            `catchAny` (pure . err . Text.pack . show)

processCommand (Fetch sha _) = FetchOk <$ processFetch sha

--------------------------------------------------------------------------------

processPush :: Bool -> Text -> Text -> RemoteHelper ProcessError CID
processPush _ localRef remoteRef = do
    root <- asks envIpfsRoot

    localRefCid <- do
        ref <- git $ Git.resolve (Git.Revision (Text.unpack localRef) [])
        maybe (throwRH $ UnknownLocalRef localRef) (pure . refToCid) ref

    remoteRefCid <- do
        refCid <- ipfs $ resolvePath (cidToText root <> "/" <> remoteRef)
        pure $ refCid >>= hush . cidFromText

    unless (Just localRefCid == remoteRefCid) $ go root localRefCid

    ipfs $ do
        -- patch link remoteRef
        root' <- patchLink root remoteRef localRefCid

        -- The remote HEAD denotes the default branch to check out. If it is not
        -- present, git clone will refuse to check out the worktree and exit with a
        -- scary error message.
        root'' <-
            getRef "HEAD" >>= \case
                Just  _ -> pure root'
                Nothing -> addObject "refs/heads/master" >>= patchLink root' "HEAD"

        root'' <$ updateRemoteUrl root'
  where
    go !root localRefCid = do
        logDebug $ fmt ("processPush: " % fcid % " " % fcid) root localRefCid
        obj <- do
            sha <-
                liftEitherRH . first CidError $
                    cidToRef @Git.SHA1 localRefCid
            logDebug $ "sha " <> Text.pack (show sha)
            dir <- Git.gitRepoPath <$> Git.getGit
            git . liftIO $ Git.looseRead dir sha

        let raw = Git.looseMarshall obj

        blkCid <- ipfs $ putBlock raw
        when (localRefCid /= blkCid) $
            throwRH . CidError $
                sfmt ( "CID mismatch:"
                     % " "
                     % "expected `" % fcid % "`"
                     % ", "
                     % "actual `" % fcid % "`"
                     ) localRefCid blkCid

        -- If the object exceeds the maximum block size, bitswap won't replicate
        -- the block. To work around this, we create a regular object and link
        -- it to the root object as @objects/<block CID>@.
        --
        -- As suggested by
        -- <https://github.com/ipfs-shipyard/git-remote-ipld/issues/12>, objects
        -- can potentially be deduplicated by storing the data separate from the
        -- header. This only makes sense for git blobs, so we don't bother for
        -- other object types.
        when (L.length raw > fromIntegral clientMaxBlockSize) $
            let
                linkName = "objects/" <> cidToText blkCid
             in
                case obj of
                    Git.ObjBlob blob -> pushLargeBlob blob root linkName
                    _                ->
                        void . ipfs $
                            addObject raw >>= patchLink root linkName

        -- process links
        forConcurrently_ clientMaxConns (objectLinks obj) $ go root

    pushLargeBlob blob root linkName =
        let
            hdr = L.fromStrict $ Git.objectWriteHeader Git.TypeBlob len
            len = fromIntegral $ L.length dat
            dat = Git.objectWrite (Git.ObjBlob blob)
         in ipfs $ do
                hdrCid <- addObject hdr
                datCid <- addObject dat
                void $
                    patchLink hdrCid "0"      datCid >>=
                    patchLink root   linkName

processFetch :: Text -> RemoteHelper ProcessError ()
processFetch sha = do
    repo <- Git.getGit
    root <- asks envIpfsRoot
    cid  <- liftEitherRH . first CidError $ cidFromHexShaText sha
    lck  <- liftIO $ newMVar ()
    lobs <- do
        env <- ask
        (>>= either throwError pure)
            . liftIO . modifyMVar (envLobs env) $ \case
                Just ls -> pure (Just ls, Right ls)
                Nothing ->
                    runRemoteHelper env (ipfs (largeObjects root)) >>= \case
                        Left  e  -> pure (Nothing, Left  e)
                        Right ls -> pure (Just ls, Right ls)

    go repo root lobs lck cid
  where
    go !repo !root !lobs lck cid = do
        ref  <- liftEitherRH . first CidError $ cidToRef @Git.SHA1 cid
        have <-
            -- Nb. mutex here as we might access the same packfile concurrently
            git . liftIO . withMVar lck . const $
                Git.getObject repo ref True
        case have of
            Just  _ -> logInfo $
                fmt ("fetch: Skipping " % fref % " (" % fcid % ")") ref cid
            Nothing -> do
                raw <- do
                    blk <- ipfs $ provideBlock lobs cid
                    case blk of
                        Just  b -> pure b
                        Nothing -> ipfs $ getBlock cid

                let obj = Git.looseUnmarshall @Git.SHA1 raw
                void . git . liftIO $
                    Git.looseWrite (Git.gitRepoPath repo) obj -- XXX: check sha matches
                forConcurrently_ clientMaxConns (objectLinks obj) $
                    go repo root lobs lck

--------------------------------------------------------------------------------

ipfs :: Monad m
     => RemoteHelperT ClientError m a
     -> RemoteHelperT ProcessError m a
ipfs = mapError IPFSError

-- XXX: hs-git uses 'error' deliberately, should be using 'tryAnyDeep' here.
-- Requires patch to upstream to get 'NFData' instances everywhere.
git :: (MonadCatch m, HasCallStack)
    => RemoteHelperT ProcessError m a
    -> RemoteHelperT ProcessError m a
git f = either throwRH pure =<< fmap (first GitError) (tryAny f)
