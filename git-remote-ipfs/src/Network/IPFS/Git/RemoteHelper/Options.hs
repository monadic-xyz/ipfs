{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.IPFS.Git.RemoteHelper.Options where

import           Control.Applicative (liftA2, (<|>))
import           Control.Exception.Safe (throwString)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Function (on)
import           Data.Git (withCurrentRepo)
import qualified Data.Git.Repository as Git
import           Data.IPLD.CID (CID, cidFromText)
import           Data.List (dropWhileEnd)
import           Data.Monoid (Last(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Generics.SOP as SOP
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Network.URI
                 ( parseURI
                 , uriAuthority
                 , uriPath
                 , uriRegName
                 , uriScheme
                 )
import           Options.Applicative
                 ( Parser
                 , ReadM
                 , argument
                 , eitherReader
                 , metavar
                 , strArgument
                 )
import           Servant.Client (BaseUrl(..), Scheme(Http), parseBaseUrl)
import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)

import           Network.IPFS.Git.RemoteHelper.Generic
                 ( HKD
                 , ginvalidate
                 , gvalidate
                 )
import           Network.IPFS.Git.RemoteHelper.Internal (note)


data Options = Options
    { optRemoteName :: String
    , optRemoteUrl  :: RemoteUrl
    }

data RemoteUrl = RemoteUrl
    { remoteUrlScheme   :: Text
    , remoteUrlIpfsPath :: IpfsPath
    }

data IpfsPath
    = IpfsPathIpfs CID
    | IpfsPathIpns Text

type IpfsOptions = IpfsOptions' SOP.I

data IpfsOptions' f = IpfsOptions
    { ipfsApiUrl       :: HKD f BaseUrl
    -- ^ URL of the IPFS daemon API.
    --
    -- Default: \"http://localhost:5001\"
    , ipfsMaxConns     :: HKD f Int
    -- ^ Maximum number of concurrent connections to the IPFS daemon. Note that
    -- this is approximate.
    --
    -- Default: 30
    , ipfsMaxBlockSize :: HKD f Int
    -- ^ The maximum size of an IPFS block. This is configurable as there is no
    -- unambiguous documentation on what the actual value is. It may also be
    -- subject to change in the future.
    --
    -- Default: 2048000 (2MB)
    } deriving Generic

instance SOP.Generic (IpfsOptions' f)

instance Semigroup (IpfsOptions' Last) where
    a <> b = IpfsOptions
        { ipfsApiUrl       = on (<>) ipfsApiUrl       a b
        , ipfsMaxConns     = on (<>) ipfsMaxConns     a b
        , ipfsMaxBlockSize = on (<>) ipfsMaxBlockSize a b
        }

instance Monoid (IpfsOptions' Last) where
    mempty  = IpfsOptions mempty mempty mempty
    mappend = (<>)

defaultIpfsOptions :: IpfsOptions
defaultIpfsOptions = IpfsOptions
    { ipfsApiUrl       = BaseUrl Http "localhost" 5001 mempty
    , ipfsMaxConns     = 30
    , ipfsMaxBlockSize = 2048000
    }

parseOptions :: Parser Options
parseOptions = liftA2 Options
    (strArgument (metavar "REMOTE_NAME"))
    (argument remoteUrl (metavar "URL"))

remoteUrl :: ReadM RemoteUrl
remoteUrl = eitherReader $ \s -> do
    uri  <- note "Invalid URI" $ parseURI s
    let path = dropWhile (== '/') $ uriPath uri
    ipfs <-
        case uriRegName <$> uriAuthority uri of
            Just "ipns" -> pure . IpfsPathIpns . Text.pack $ path
            _           -> IpfsPathIpfs <$> cidFromString path
    pure RemoteUrl
        { remoteUrlScheme   = Text.pack . dropWhileEnd (== ':') $ uriScheme uri
        , remoteUrlIpfsPath = ipfs
        }
  where
    cidFromString = cidFromText . \case
        [] -> emptyRepo
        xs -> Text.pack xs

    emptyRepo = "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn"

-- | Determine the 'IpfsOptions'.
--
-- This must happen after 'parseOptions' in order to support per-remote
-- settings. The @GIT_DIR@ environment variable must be set and point to a valid
-- git repository (when the remote helper is invoked by git, this is the current
-- repository).
--
-- 'IpfsOptions' are configured using @git-config(2)@. The precedence rules
-- specified there apply. However, @$XDG_CONFIG_HOME/git/config@ and
-- @$(prefix)/etc/gitconfig@ (i.e. @--system@) are __not__ yet supported.
--
-- The available configuration keys are:
--
-- * @ipfs.apiurl@
-- * @ipfs.maxconnections@
-- * @ipfs.maxblocksize@
--
-- 'ipfsApiUrl' may be overridden per-remote using the key @remote.<remote
-- name>.ipfsapiurl@ (e.g. @remote.origin.ipfsapiurl@). If the environment
-- variable @IPFS_API_URL@, it will be used instead of any @git-config@
-- settings.
--
getIpfsOptions :: HasCallStack => Options -> IO IpfsOptions
getIpfsOptions Options { optRemoteName } = withCurrentRepo $ \r -> do
    ipfsApiUrl <-
        fmap Last . runMaybeT $ do
            url <-
                    MaybeT (lookupEnv "IPFS_API_URL")
                <|> MaybeT (Git.configGet r ("remote \"" <> optRemoteName <> "\"") "ipfsapiurl")
                <|> MaybeT (Git.configGet r "ipfs" "apiurl")
            parseBaseUrl url

    ipfsMaxConns <-
        Last . (>>= readMaybe) <$> Git.configGet r "ipfs" "maxconnections"
    ipfsMaxBlockSize <-
        Last . (>>= readMaybe) <$> Git.configGet r "ipfs" "maxblocksize"

    maybe (throwString "Das Unmögliche ist eingetreten: Invalid IpfsOptions")
          pure
        . getLast
        . gvalidate
        $ ginvalidate pure defaultIpfsOptions <> IpfsOptions {..}
