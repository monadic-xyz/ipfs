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

    , clientMaxConns
    , clientMaxBlockSize

    , listPaths
    , getRef
    , resolvePath
    , patchLink
    , putBlock
    , addObject
    , largeObjects
    , provideBlock
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
import           Formatting (sformat, stext, (%))
import           System.FilePath (joinPath)
import           System.Process.Typed (runProcess_, shell)

import           Servant.API
import           Servant.Client (ServantError(..))
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

-- FIXME(kim): We may want this to be configurable somehow
clientMaxConns :: Int
clientMaxConns = 30

clientMaxBlockSize :: Int
clientMaxBlockSize = 2048000

listPaths
    :: MonadIO m
    => Text
    -> Word
    -> RemoteHelperT ClientError m [RefPath]
listPaths path !level = do
    logDebug $ "listPaths: " <> path
    refs <- ipfsList path Nothing Nothing
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
                Just (-1) -> pure . maybeToList $ do
                    name <- Lens.firstOf nameL link
                    hash <- Lens.firstOf hashL link
                    pure RefPath
                        { refPathPath = Text.unpack $ path <> "/" <> name
                        , refPathType = RefPathHead
                        , refPathHash = hash
                        }

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

provideBlock
    :: (MonadCatch m, MonadIO m)
    => HashMap CID CID
    -> CID
    -> RemoteHelperT ClientError m (Maybe L.ByteString)
provideBlock largeObjs cid =
    for (Map.lookup cid largeObjs) $ \cid' -> do
        contents <-
            stream $ ipfsCat ("/ipfs/" <> cidToText cid') Nothing Nothing

        -- XXX: go checks cid here by issuing DagPut with the data (reuploading
        -- what we just got??). Is this necessary? Should the DagPut occur when
        -- pushing?
        dag <- ipfsDagPut contents (Just "raw") (Just "git") Nothing Nothing
        either throwRH pure $ do
            txt     <-
                note (InvalidResponse "ipfsDagPut: expected CID in response" dag) $
                    Lens.firstOf cidL dag
            realCid <- first CidError $ cidFromText txt
            if realCid == cid' then
                pure ()
            else
                let
                    f = "Unexpected CID for provided block: " % ftxt % " " % fcid
                 in
                    Left $ CidError (sfmt f txt cid')

        pure contents

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
        ipfsAdd bs'
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing

getBlock
    :: (MonadCatch m, MonadIO m)
    => CID
    -> RemoteHelperT ClientError m L.ByteString
getBlock cid = stream $ ipfsBlockGet (cidToText cid)

updateRemoteUrl :: MonadIO m => CID -> RemoteHelperT ClientError m ()
updateRemoteUrl root = do
    url <- asks $ optRemoteUrl . envOptions
    case remoteUrlIpfsPath url of
        IpfsPathIpns name -> viaIpns name
        IpfsPathIpfs _    -> viaConfig (remoteUrlScheme url) root
  where
    viaIpns name = do
        logInfo "Updating IPNS"
        let ipnsTarget = "/ipfs/" <> cidToText root
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
                    (sformat ( "ipfsNamePublish: expected name "
                             % "`" % stext % "` "
                             % "pointing to `" % stext % "`"
                             ) name ipnsTarget)
                    res

    viaConfig scheme cid = do
        remoteName <- asks $ Text.pack . optRemoteName . envOptions
        let
            configKey = "remote." <> remoteName <> ".url"
            remoteUrl = scheme <> "://ipfs/" <> cidToText cid
         in do
            logInfo $ "Updating " <> configKey
            runProcess_ . shell . Text.unpack $
                "git config " <> configKey <> " " <> remoteUrl

-- lenses

cidL :: Lens.AsValue t => Lens.Traversal' t Text
cidL = Lens.key "Cid" . Lens.key "/" . Lens._String

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

-- brilliant API design
isNoLink :: ServantError -> Bool
isNoLink = \case
    FailureResponse res ->
        case Lens.firstOf messageL (Servant.responseBody res) of
            Just  m | "no link named" `Text.isPrefixOf` m -> True
            _                                             -> False
    _ -> False

-- | Subset of IPFS API needed for remote helper
type IPFS =
         ApiV0Add
    :<|> ApiV0BlockPut
    :<|> ApiV0DagPut
    :<|> ApiV0Ls
    :<|> ApiV0ObjectPatchAddLink
    :<|> ApiV0Resolve
    :<|> ApiV0NamePublish

ipfsAdd
    :<|> ipfsBlockPut
    :<|> ipfsDagPut
    :<|> ipfsList
    :<|> ipfsObjectPatchAddLink
    :<|> ipfsResolve
    :<|> ipfsNamePublish
    = client
  where
    client = Servant.hoistClient api nat (Servant.client api)

    nat m = asks envClient
        >>= liftIO . Servant.runClientM m
        >>= either (throwRH . ApiError) pure

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
    flip catches handlers $
        liftIO . ServantS.withClientM m env $ \case
            Left  e -> throwM e
            Right s -> runExceptT (runSourceT s) >>= \case
                Left  e'  -> throwString e'
                Right bss -> pure $ L.fromChunks bss
  where
    handlers =
        [ Handler $ throwRH . ApiError
        , Handler $ \(StringException e _) -> throwRH $ StreamingError e
        ]
