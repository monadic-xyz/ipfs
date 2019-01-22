{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Network.IPFS.Git.RemoteHelper
    ( ProcessError
    , renderProcessError

    , processCommand
    )
where

import           Control.Exception.Safe
                 ( MonadCatch
                 , MonadMask
                 , SomeException
                 , catchAny
                 , tryAny
                 )
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as L
import           Data.Foldable (toList, traverse_)
import           Data.IORef (atomicModifyIORef')
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Read as Text
import           Data.Traversable (for)
import           System.FilePath (joinPath, splitDirectories)

import           Data.IPLD.CID (CID, cidFromText, cidToText)

import qualified Data.Git.Monad as Git
import qualified Data.Git.Ref as Git
import qualified Data.Git.Revision as Git
import qualified Data.Git.Storage as Git
import qualified Data.Git.Storage.Loose as Git

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

renderProcessError :: ProcessError -> Text
renderProcessError = \case
    GitError  e       -> fmt ("Error accessing git repo: " % shown) e
    IPFSError e       -> renderClientError e
    CidError  e       -> fmt ("Cid conversion error: " % fstr) e
    UnknownLocalRef r -> fmt ("Ref not found locally: " % ftxt) r

processCommand
    :: (MonadMask m, MonadIO m)
    => Command
    -> RemoteHelperT ProcessError m CommandResult
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
            fmap catMaybes . for branches $ \branch ->
                ipfs $ resolvePath (cidToText root <> "/" <> branch)

        traverse (liftEitherRH . first CidError . hexShaFromCidText) cids

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

processPush
    :: (MonadMask m, MonadIO m)
    => Bool
    -> Text
    -> Text
    -> RemoteHelperT ProcessError m CID
processPush _ localRef remoteRef = do
    root <- asks envIpfsRoot

    localHeadRef <- do
        ref <- git $ Git.resolve (Git.Revision (Text.unpack localRef) [])
        maybe (throwRH $ UnknownLocalRef localRef) (pure . refToCid) ref

    remoteHeadRef <- do
        refCid <- ipfs $ resolvePath (cidToText root <> "/" <> remoteRef)
        pure $ refCid >>= hush . cidFromText

    go root remoteHeadRef localHeadRef

    -- patch link remoteRef
    root' <- ipfs $ patchLink root remoteRef localHeadRef

    -- The remote HEAD denotes the default branch to check out. If it is not
    -- present, git clone will refuse to check out the worktree and exit with a
    -- scary error message.
    hEAD <- ipfs $ getRef "HEAD"
    root'' <-
        case hEAD of
            Just  _ -> pure root'
            Nothing -> linkedObject root' "HEAD" "refs/heads/master"
    root'' <$ ipfs (updateRemoteUrl root'')
  where
    go !root remoteHeadRef localHeadRef
        | Just localHeadRef == remoteHeadRef = pure ()
        | otherwise = do

        logDebug $
            fmt ("processPush: " % fcid % " " % fcid % " " % shown)
                root
                localHeadRef
                (cidToRef @Git.SHA1 <$> remoteHeadRef)

        obj <- do
            sha  <- liftEitherRH . first CidError $ cidToRef localHeadRef
            logDebug $ "sha " <> Text.pack (show sha)
            localHeadRef' <- git $ Git.resolve (sha :: Git.Ref Git.SHA1)
            case localHeadRef' of
                Nothing -> throwRH $
                    UnknownLocalRef $ Text.pack (Git.toHexString sha)
                Just  r -> do
                    dir <- Git.gitRepoPath <$> Git.getGit
                    git . liftIO $ Git.looseRead dir r

        let raw = Git.looseMarshall obj

        logDebug $ "BlockPut: " <> decodeUtf8With lenientDecode (L.toStrict raw)
        blockCid <- do
            cid <- ipfs $ putBlock raw
            -- check 'res' CID matches SHA
            when (localHeadRef /= cid) $
                throwRH
                    . CidError
                    $ sfmt ("CID mismatch: expected `" % fcid % "`, actual `" % fcid % "`")
                           localHeadRef
                           cid
            pure cid

        logDebug $ fmt ("blockCid: " % fcid) blockCid

        -- if loose object > 2048k, create object + link block to it
        objCid <-
            if L.length raw > 2048000 then
                linkedObject root ("objects/" <> cidToText blockCid) raw
            else
                pure blockCid

        logDebug $ fmt ("objCid: " % fcid) objCid
        -- process links
        traverse_ (go root remoteHeadRef) (objectLinks obj)

    linkedObject base name raw = ipfs $ addObject raw >>= patchLink base name

processFetch
    :: (MonadCatch m, MonadIO m)
    => Text
    -> RemoteHelperT ProcessError m ()
processFetch sha = do
    repo  <- Git.gitRepoPath <$> Git.getGit
    root  <- asks envIpfsRoot
    cid   <- liftEitherRH . first CidError $ cidFromHexShaText sha
    lobjs <- ipfs $ largeObjects root -- XXX: load lobjs only once
    go repo root lobjs cid
  where
    go repo root lobjs cid = do
        ref  <- liftEitherRH . first CidError $ cidToRef @Git.SHA1 cid
        have <- Git.getGit >>= \g -> git . liftIO $ Git.getObject g ref True
        case have of
            Just  _ ->
                logInfo $ fmt ("fetch: Skipping " % fref % " (" % fcid % ")") ref cid
            Nothing -> do
                raw <- do
                    blk <- ipfs $ provideBlock lobjs cid
                    case blk of
                        Just  b -> pure b
                        Nothing -> ipfs $ getBlock cid

                let obj = Git.looseUnmarshall @Git.SHA1 raw
                void . git . liftIO $ Git.looseWrite repo obj -- XXX: check sha matches
                traverse_ (go repo root lobjs) (objectLinks obj)

--------------------------------------------------------------------------------

ipfs :: Monad m
     => RemoteHelperT ClientError m a
     -> RemoteHelperT ProcessError m a
ipfs = mapError IPFSError

-- XXX: hs-git uses 'error' deliberately, should be using 'tryAnyDeep' here.
-- Requires patch to upstream to get 'NFData' instances everywhere.
git :: MonadCatch m
    => RemoteHelperT ProcessError m a
    -> RemoteHelperT ProcessError m a
git f = either throwRH pure =<< fmap (first GitError) (tryAny f)
