{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Network.IPFS.Git.RemoteHelper.Client
    ( RefPath (..)
    , RefPathType (..)

    , ClientError
    , renderClientError

    , listPaths
    , getRef
    , resolvePath
    , patchLink
    , putBlock
    , addObject
    , pin
    , largeObjects
    , provideLargeObject
    , getBlock
    , updateRemoteUrl
    )
where

import           Control.Applicative (liftA2)
import           Control.Exception.Safe
import qualified Control.Lens as Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Lens
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L (ByteString, fromChunks, null)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT (decodeUtf8)
import           Data.Traversable (for)
import           System.FilePath (joinPath)
import           System.Process.Typed (runProcess_, shell)

import           Servant.API
import qualified Servant.Client as Servant
import qualified Servant.Client.Streaming as ServantS
import           Servant.Types.SourceT

import           Data.IPLD.CID (CID, cidFromText, cidToText)
import           Network.IPFS.API

import           Network.IPFS.Git.RemoteHelper.Format
import           Network.IPFS.Git.RemoteHelper.Internal
import           Network.IPFS.Git.RemoteHelper.Options
                 ( IpfsPath(..)
                 , optRemoteName
                 , optRemoteUrl
                 , remoteUrlIpfsPath
                 , remoteUrlScheme
                 )
import           Network.IPFS.Git.RemoteHelper.Trans

#if MIN_VERSION_servant_client(0,16,0)
type ServantError = Servant.ClientError
#else
type ServantError = Servant.ServantError
#endif

data ClientError
    = ApiError ServantError
    | InvalidResponse Text Aeson.Value
    | CidError String
    | StreamingError String
    deriving Show

instance DisplayError ClientError where
    displayError = renderClientError

renderClientError :: ClientError -> Text
renderClientError = \case
    ApiError        e       -> fmt ("Error talking to IPFS: " % shown) e
    InvalidResponse msg raw -> fmt (ftxt % " -- " % shown) msg raw
    CidError        msg     -> fmt ("Cid conversion error: " % fstr) msg
    StreamingError  msg     -> fmt ("SourceT yielded error: " % fstr) msg

data RefPath = RefPath
    { refPathPath :: FilePath
    , refPathType :: RefPathType
    , refPathHash :: Text
    }

data RefPathType = RefPathRef | RefPathHead

listPaths
    :: MonadIO m
    => Text
    -> Word
    -> RemoteHelperT ClientError m [RefPath]
listPaths path !level = do
    logDebug $ "listPaths: " <> path
    refs <- ipfsList path Nothing (Just True)
    logDebug $ "listPaths: " <> Text.pack (show refs)
    fmap concat <$> for (Lens.toListOf linksL refs) $ \link ->
            case Lens.firstOf typeL link of
                Nothing -> throwRH $
                    InvalidResponse "ipfsList: missing link type" refs
                -- directory:
                Just  1 ->
                    case Lens.firstOf nameL link of
                        Just "objects" | level == 0 -> pure []
                        Just name      -> listPaths (path <> "/" <> name) (level + 1)
                        Nothing        -> pure [] -- ??
                -- file:
                Just  2 -> pure . maybeToList $ do
                    name <- Lens.firstOf nameL link
                    hash <- Lens.firstOf hashL link
                    pure RefPath
                        { refPathPath = Text.unpack $ path <> "/" <> name
                        , refPathType = RefPathRef
                        , refPathHash = hash
                        }
                -- unknown (assume head):
                --
                -- Nb.: unknown is 0 in ipfs version >= 0.4.19, and -1 in
                -- earlier versions.
                Just x | x == 0 || x == -1 -> pure . maybeToList $ do
                    name <- Lens.firstOf nameL link
                    hash <- Lens.firstOf hashL link
                    pure RefPath
                        { refPathPath = Text.unpack $ path <> "/" <> name
                        , refPathType = RefPathHead
                        , refPathHash = hash
                        }
                -- Post-0.4.19, this can only be @TSymlink@, previously .. idk.
                -- Either way, we don't know what to do.
                Just x -> throwRH $
                    InvalidResponse (fmt ("Unexpected link type: " % shown) x) refs

getRef
    :: (MonadCatch m, MonadIO m)
    => FilePath
    -> RemoteHelperT ClientError m (Maybe Text)
getRef name = do
    root <- asks $ Text.unpack . cidToText . envIpfsRoot

    let path = Text.pack $ joinPath [root, name]
    logDebug $ "getRef: " <> path
    bs <- stream (ipfsCat path Nothing Nothing) `catchRH` noLink
    if | L.null bs -> pure Nothing
       | otherwise -> pure . Just . LT.toStrict $ LT.decodeUtf8 bs
  where
    noLink (ApiError e) | isNoLink e = pure mempty
    noLink e            = throwRH e

resolvePath :: MonadIO m => Text -> RemoteHelperT ClientError m (Maybe Text)
resolvePath p = do
    logDebug $ "Resolve path: " <> p
    res <- ipfsResolve p Nothing Nothing Nothing `catchRH` noLink
    pure . fmap strip $ Lens.firstOf pathL res
  where
    noLink (ApiError e) | isNoLink e = pure Aeson.Null
    noLink e            = throwRH e

    strip x = fromMaybe x $ Text.stripPrefix "/ipfs/" x

patchLink :: MonadIO m => CID -> Text -> CID -> RemoteHelperT ClientError m CID
patchLink from name to = do
    logDebug $ fmt ("patchLink " % fcid % " " % ftxt % " " % fcid) from name to
    res <- ipfsObjectPatchAddLink (cidToText from) name (cidToText to) (Just True)
    either throwRH pure $ do
        hash <- note (invalidResponse res) $ Lens.firstOf hashL res
        first CidError $ cidFromText hash
  where
    invalidResponse =
        InvalidResponse "ipfsObjectPatchAddLink: expected 'Hash' key"

largeObjects :: MonadIO m => CID -> RemoteHelperT ClientError m (HashMap CID CID)
largeObjects root = do
    res <-
        ipfsList (cidToText root <> "/objects") Nothing Nothing `catchRH` noLink
    either throwRH (pure . Map.fromList)
        . for (Lens.toListOf linksL res) $ \link -> do
            hash <- toCid =<<
                note (invalidResponse "Hash" res) (Lens.firstOf hashL link)
            name <- toCid =<<
                note (invalidResponse "Link" res) (Lens.firstOf nameL link)
            pure (name, hash)
  where
    noLink (ApiError e) | isNoLink e = pure Aeson.Null
    noLink e            = throwRH e

    invalidResponse =
        InvalidResponse . fmt ("ipfsList: expected '" % ftxt % "' key")

    toCid = first CidError . cidFromText

provideLargeObject
    :: (MonadCatch m, MonadIO m)
    => HashMap CID CID
    -> CID
    -> RemoteHelperT ClientError m (Maybe L.ByteString)
provideLargeObject largeObjs cid =
    for (Map.lookup cid largeObjs) $ \cid' ->
        stream $ ipfsCat ("/ipfs/" <> cidToText cid') Nothing Nothing

putBlock :: MonadIO m => L.ByteString -> RemoteHelperT ClientError m CID
putBlock bs = do
    res <- ipfsBlockPut bs (Just "git-raw") (Just "sha1") Nothing
    either throwRH pure $ do
        key <- note (invalidResponse res) $ Lens.firstOf keyL res
        first CidError $ cidFromText key
  where
    invalidResponse = InvalidResponse "ipfsBlockPut: expected 'Key' key"

addObject :: MonadIO m => L.ByteString -> RemoteHelperT ClientError m CID
addObject bs = do
    res <- ipfsAdd' bs
    liftEitherRH $ do
        sha <- note (invalidResponse res) $ Lens.firstOf hashL res
        first CidError $ cidFromText sha
  where
    invalidResponse = InvalidResponse "ipfsAdd: expected 'Hash' key"

    ipfsAdd' bs' =
        ipfsAdd bs'         -- data
                Nothing     -- recursive
                Nothing     -- quiet
                Nothing     -- quieter
                Nothing     -- silent
                Nothing     -- progress
                Nothing     -- trickle
                Nothing     -- only-hash
                Nothing     -- wrap-with-directory
                Nothing     -- hidden
                Nothing     -- chunker
                (Just True) -- pin
                Nothing     -- raw-leaves
                Nothing     -- nocopy
                Nothing     -- fscache
                Nothing     -- cid-version
                Nothing     -- hash function

pin :: MonadIO m => CID -> RemoteHelperT ClientError m [CID]
pin cid = do
    res <- ipfsPinAdd (cidToText cid)
                      (Just True)     -- recursive
                      (Just False)    -- progress
    liftEitherRH $
        traverse (first CidError . cidFromText) $ Lens.toListOf pinsL res

getBlock
    :: (MonadCatch m, MonadIO m)
    => CID
    -> RemoteHelperT ClientError m L.ByteString
getBlock cid = stream $ ipfsBlockGet (cidToText cid)

updateRemoteUrl :: MonadIO m => CID -> RemoteHelperT ClientError m ()
updateRemoteUrl root = do
    url    <- asks $ optRemoteUrl . envOptions
    remote <- asks $ Text.pack . optRemoteName . envOptions

    case remoteUrlIpfsPath url of
        IpfsPathIpns name -> viaIpns name
        IpfsPathIpfs _    -> viaConfig (remoteUrlScheme url) remote root

    -- backup plan in case the IPNS link expires
    updateConfig ("remote." <> remote <> ".lastseencid") (cidToText root)
  where
    viaIpns name = do
        let ipnsTarget = "/ipfs/" <> cidToText root
        logInfo $
            fmt ("Updating IPNS link " % ftxt % " to " % ftxt) name ipnsTarget
        res <-
            ipfsNamePublish ipnsTarget
                            (Just True)       -- resolve
                            (Just "2540400h") -- lifetime
                            Nothing           -- ttl (caching)
                            (Just name)       -- key

        case liftA2 (\name' root' -> name' == name && root' == ipnsTarget)
                    (Lens.firstOf nameL  res)
                    (Lens.firstOf valueL res) of
            Just True -> pure ()
            _         -> throwRH $
                InvalidResponse
                    (fmt ( "ipfsNamePublish: expected name "
                         % "`" % ftxt % "` "
                         % "pointing to `" % ftxt % "`"
                         ) name ipnsTarget)
                    res

    viaConfig scheme remoteName cid =
        let
            configKey = "remote." <> remoteName <> ".url"
            remoteUrl = scheme <> "://ipfs/" <> cidToText cid
         in
            updateConfig configKey remoteUrl

    updateConfig key value = do
        logInfo $ fmt ("Updating " % ftxt % " to " % ftxt) key value
        runProcess_ . shell . Text.unpack $
            "git config " <> key <> " " <> value

-- lenses

linksL :: Lens.AsValue t => Lens.Traversal' t Aeson.Value
linksL = Lens.key "Objects" . Lens.nth 0 . Lens.key "Links" . Lens.values

typeL :: Lens.AsValue t => Lens.Traversal' t Int
typeL = Lens.key "Type" . Lens._Integral

nameL :: Lens.AsValue t => Lens.Traversal' t Text
nameL = Lens.key "Name" . Lens._String

messageL :: Lens.AsValue t => Lens.Traversal' t Text
messageL = Lens.key "Message" . Lens._String

hashL :: Lens.AsValue t => Lens.Traversal' t Text
hashL = Lens.key "Hash" . Lens._String

pathL :: Lens.AsValue t => Lens.Traversal' t Text
pathL = Lens.key "Path" . Lens._String

keyL :: Lens.AsValue t => Lens.Traversal' t Text
keyL = Lens.key "Key" . Lens._String

valueL :: Lens.AsValue t => Lens.Traversal' t Text
valueL = Lens.key "Value" . Lens._String

pinsL :: Lens.AsValue t => Lens.IndexedTraversal' Int t Text
pinsL = Lens.key "Pins" . Lens.values . Lens._String

-- brilliant API design
isNoLink :: ServantError -> Bool
isNoLink = \case
#if MIN_VERSION_servant_client(0,16,0)
    Servant.FailureResponse _ res ->
#else
    Servant.FailureResponse   res ->
#endif
        case Lens.firstOf messageL (Servant.responseBody res) of
            Just  m | "no link named" `Text.isPrefixOf` m -> True
            _                                             -> False
    _ -> False

-- | Subset of IPFS API needed for remote helper
type IPFS =
         ApiV0Add
    :<|> ApiV0BlockPut
    :<|> ApiV0Ls
    :<|> ApiV0ObjectPatchAddLink
    :<|> ApiV0Resolve
    :<|> ApiV0NamePublish
    :<|> ApiV0PinAdd

ipfsAdd
    :<|> ipfsBlockPut
    :<|> ipfsList
    :<|> ipfsObjectPatchAddLink
    :<|> ipfsResolve
    :<|> ipfsNamePublish
    :<|> ipfsPinAdd
    = client
  where
    client = Servant.hoistClient api nat (Servant.client api)

    nat m = do
        env <- asks envClient
        either (throwRH . ApiError) pure =<< liftIO (Servant.runClientM m env)

    api = Proxy @IPFS

-- | Streaming endpoints
type IPFS' = ApiV0BlockGet :<|> ApiV0Cat

ipfsBlockGet :<|> ipfsCat = ServantS.client (Proxy @IPFS')

stream
    :: (MonadCatch m, MonadIO m)
    => ServantS.ClientM (SourceT IO BS.ByteString)
    -> RemoteHelperT ClientError m L.ByteString
stream m = do
    env <- asks envClient
    liftIO (go env) `catches` handlers
  where
    handlers =
        [ Handler $ throwRH . ApiError
        , Handler $ \(StringException e _) -> throwRH $ StreamingError e
        ]

    go env =
        ServantS.withClientM m env $ \case
            Left  e -> throwM e
            Right s -> runExceptT (runSourceT s) >>= \case
                Left  e'  -> throwString e'
                Right bss -> pure $ L.fromChunks bss

