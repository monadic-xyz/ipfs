{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Network.IPFS.Git.RemoteHelper
    ( ProcessError
    , renderProcessError

    , processCommand
    )
where

import           Control.Concurrent.MVar (modifyMVar)
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
import           Data.Foldable (toList, traverse_)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as Set
import           Data.IORef (atomicModifyIORef')
import           Data.Maybe (catMaybes, isNothing)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import           Data.Traversable (for)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           GHC.Stack (HasCallStack)
import           System.FilePath (joinPath, splitDirectories)

import           Data.Conduit
import qualified Data.Conduit.Combinators as Conduit

import           Data.IPLD.CID (CID, cidFromText, cidToText)

import qualified Data.Git.Monad as Git (getGit, liftGit)
import           Data.Git.Ref (SHA1)
import qualified Data.Git.Ref as Git (Ref)
import qualified Data.Git.Repository as Git (branchList, resolveRevision)
import qualified Data.Git.Revision as Git (Revision(..))
import qualified Data.Git.Storage as Git (Git, getObject, getObject_, setObject)
import qualified Data.Git.Storage.Loose as Git (looseMarshall, looseUnmarshall)
import qualified Data.Git.Storage.Object as Git
                 ( Object(ObjBlob)
                 , ObjectType(TypeBlob)
                 , objectWrite
                 , objectWriteHeader
                 )

import           Network.IPFS.Git.RemoteHelper.Client
import           Network.IPFS.Git.RemoteHelper.Command
import           Network.IPFS.Git.RemoteHelper.Format
import           Network.IPFS.Git.RemoteHelper.Internal
import           Network.IPFS.Git.RemoteHelper.Options (IpfsOptions'(..))
import           Network.IPFS.Git.RemoteHelper.Trans


data ProcessError
    = GitError        SomeException
    | IPFSError       ClientError
    | CidError        String
    | UnknownLocalRef Text
    | HashError       HashMismatch
    deriving Show

-- | Indicate two hashes expected to be equal aren't.
--
-- The data constructors take the expected value first, then the actual.
data HashMismatch
    = CidMismatch CID CID
    | RefMismatch (Git.Ref SHA1) (Git.Ref SHA1)
    deriving Show

instance DisplayError ProcessError where
    displayError = renderProcessError

renderProcessError :: ProcessError -> Text
renderProcessError = \case
    GitError  e       -> fmt ("Error accessing git repo: " % shown) e
    IPFSError e       -> renderClientError e
    CidError  e       -> fmt ("Cid conversion error: " % fstr) e
    UnknownLocalRef r -> fmt ("Ref not found locally: " % ftxt) r
    HashError  e      -> renderHashMismatch e

renderHashMismatch :: HashMismatch -> Text
renderHashMismatch (CidMismatch e a) =
    fmt ("Cid mismatch: expected `" % fcid % "`, actual: `" % fcid % "`") e a
renderHashMismatch (RefMismatch e a) =
    fmt ("Ref mismatch: expected `" % fref % "`, actual: `" % fref % "`") e a

processCommand
    :: HasCallStack
    => Command
    -> RemoteHelper ProcessError CommandResult
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
    branches <-
        map (fmt $ "refs/heads/" % frefName) . toList <$> git Git.branchList
    logDebug $ fmt ("list for-push: branches: " % shown) branches

    remoteRefs <- do
        cids <-
            forConcurrently branches $ \branch ->
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

processPush
    :: HasCallStack
    => Bool
    -> Text
    -> Text
    -> RemoteHelper ProcessError CID
processPush _ localRef remoteRef = do
    root <- asks envIpfsRoot

    localRefCid <- do
        ref <- git $ flip Git.resolveRevision (Git.Revision (Text.unpack localRef) [])
        maybe (throwRH $ UnknownLocalRef localRef) (pure . refToCid) ref

    remoteRefCid <- do
        refCid <- ipfs $ resolvePath (cidToText root <> "/" <> remoteRef)
        pure $ refCid >>= hush . cidFromText

    maxConc <- asks $ ipfsMaxConns . envIpfsOptions
    runConduit $
           collectObjects (maybe mempty Set.singleton remoteRefCid)
                          (Vector.singleton localRefCid)
        .| Conduit.conduitVector maxConc
        .| Conduit.mapM_ (\(batch :: Vector (CID, Git.Object SHA1)) ->
               forConcurrently_ batch $ pushObject root)

    ipfs $ do
        -- Update the root to point to the local ref, up to which we just pushed
        root' <- patchLink root remoteRef localRefCid
        -- The remote HEAD denotes the default branch to check out. If it is not
        -- present, git clone will refuse to check out the worktree and exit
        -- with a scary error message.
        linkedObject "refs/heads/master" root' "HEAD" >>= \hEAD ->
            -- HEAD is our new root, update the remote.url and pin
            hEAD <$ concurrently_ (updateRemoteUrl hEAD) (pin hEAD)
  where
    collectObjects _    cids | Vector.null cids = pure ()
    collectObjects seen cids = do
        let cid   = Vector.unsafeHead cids
        let cids' = Vector.unsafeTail cids

        if | Set.member cid seen -> collectObjects seen cids'
           | otherwise           -> do

            obj <- lift $ do
                sha <- liftEitherRH . first CidError $ cidToRef @SHA1 cid
                git $ \repo -> Git.getObject_ repo sha True

            yield (cid, obj)

            collectObjects (Set.insert cid seen) (objectLinks obj <> cids')

    pushObject root (cid, obj) = do
        let raw = Git.looseMarshall obj

        logDebug $ fmt ("Pushing " % fcid) cid
        blkCid <- ipfs $ putBlock raw
        when (cid /= blkCid) $
            throwRH $ HashError (CidMismatch cid blkCid)

        -- If the object exceeds the maximum block size, bitswap won't replicate
        -- the block. To work around this, we create a regular object and link
        -- it to the root object as @objects/<block CID>@.
        --
        -- As suggested by
        -- <https://github.com/ipfs-shipyard/git-remote-ipld/issues/12>, objects
        -- can potentially be deduplicated by storing the data separate from the
        -- header. This only makes sense for git blobs, so we don't bother for
        -- other object types.
        maxBlockSize <- asks $ fromIntegral . ipfsMaxBlockSize . envIpfsOptions
        when (L.length raw > maxBlockSize) $ do
            let linkName = "objects/" <> cidToText blkCid
            void . ipfs $
                case obj of
                    Git.ObjBlob blob -> pushLargeBlob blob root linkName
                    _                -> linkedObject  raw  root linkName

    pushLargeBlob blob root linkName =
        let
            hdr = L.fromStrict $ Git.objectWriteHeader Git.TypeBlob len
            len = fromIntegral $ L.length dat
            dat = Git.objectWrite (Git.ObjBlob blob)
         in do
            hdrCid <- addObject hdr
            datCid <- addObject dat
            patchLink hdrCid "0" datCid >>= patchLink root linkName

    linkedObject bytes root linkName =
        addObject bytes >>= patchLink root linkName

processFetch :: HasCallStack => Text -> RemoteHelper ProcessError ()
processFetch sha = do
    cid  <- liftEitherRH . first CidError $ cidFromHexShaText sha
    lobs <- loadLobs

    runConduit $
           fetchObjects lobs (Vector.singleton cid)
        .| Conduit.mapM_ storeObject

    void $ asks envIpfsRoot >>= ipfs . pin
  where
    fetchObjects
        :: HashMap CID CID
        -> Vector CID
        -> ConduitT () (Git.Ref SHA1, Git.Object SHA1) (RemoteHelper ProcessError) ()
    fetchObjects !lobs cids = do
        todo <- lift $ do
            xs <-
                for cids $ \cid -> do
                    ref <- liftEitherRH . first CidError $ cidToRef @SHA1 cid
                    pure (ref, cid)
            Vector.filterM (fmap isNothing . lookupObject . fst) xs

        unless (Vector.null todo) $ do
            objs <- lift $ do
                logDebug $ fmt ("Fetching objects: " % shown) todo
                maxConc <- asks $ ipfsMaxConns . envIpfsOptions
                fmap join . for (chunksOfV maxConc todo) $ \batch ->
                    forConcurrently batch (fetchObject lobs . snd)
            traverse_ yield $
                Vector.zip (Vector.map fst todo) objs
            fetchObjects lobs $ foldMap objectLinks objs

    fetchObject lobs cid = ipfs $ do
        lob <- provideLargeObject lobs cid
        Git.looseUnmarshall @SHA1 <$> maybe (getBlock cid) pure lob

    storeObject (ref, obj) = do
        ref' <- git $ flip Git.setObject obj
        when (ref' /= ref) $
            throwRH $ HashError (RefMismatch ref ref')

    lookupObject ref = git $ \repo -> Git.getObject repo ref True

    loadLobs = do
        env <- ask
        (>>= either throwError pure)
            . liftIO . modifyMVar (envLobs env) $ \case
                Just ls -> pure (Just ls, Right ls)
                Nothing ->
                    runRemoteHelper env (ipfs (largeObjects (envIpfsRoot env))) >>= \case
                        Left  e  -> pure (Nothing, Left  e)
                        Right ls -> pure (Just ls, Right ls)

--------------------------------------------------------------------------------

ipfs :: Monad m
     => RemoteHelperT ClientError m a
     -> RemoteHelperT ProcessError m a
ipfs = mapError IPFSError

-- XXX: hs-git uses 'error' deliberately, should be using 'tryAnyDeep' here.
-- Requires patch to upstream to get 'NFData' instances everywhere.
git :: (MonadCatch m, MonadIO m, HasCallStack)
    => (Git.Git SHA1 -> IO a)
    -> RemoteHelperT ProcessError m a
git f = do
    repo <- Git.getGit
    res  <- Git.liftGit $ first GitError <$> tryAny (f repo)
    either throwRH pure res

chunksOfV :: Int -> Vector a -> Vector (Vector a)
chunksOfV n = Vector.unfoldr go
  where
    go v | Vector.null v = Nothing
         | otherwise     = Just $ Vector.splitAt n v
