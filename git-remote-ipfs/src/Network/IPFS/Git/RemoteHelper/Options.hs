{-# LANGUAGE OverloadedStrings #-}

module Network.IPFS.Git.RemoteHelper.Options where

import           Data.Bifunctor (first)
import           Data.IPLD.CID (CID, cidFromText)
import           Data.List (dropWhileEnd)
import           Data.Text (Text)
import qualified Data.Text as Text
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
                 , help
                 , long
                 , metavar
                 , option
                 , showDefault
                 , strArgument
                 , value
                 )
import           Servant.Client (BaseUrl(..), Scheme(..), parseBaseUrl)

import           Network.IPFS.Git.RemoteHelper.Internal (note)


data Options = Options
    { optRemoteName     :: String
    , optRemoteUrl      :: RemoteUrl
    , optIpfsApiBaseUrl :: BaseUrl
    }

data RemoteUrl = RemoteUrl
    { remoteUrlScheme   :: Text
    , remoteUrlIpfsPath :: IpfsPath
    }

data IpfsPath
    = IpfsPathIpfs CID
    | IpfsPathIpns Text

parseOptions :: Parser Options
parseOptions = Options
    <$> strArgument (metavar "REMOTE_NAME")
    <*> argument remoteUrl (metavar "URL")
    <*> option (eitherReader (first show . parseBaseUrl))
        ( long  "ipfs-url"
       <> help  "Base URL of the IPFS API"
       <> value (BaseUrl Http "localhost" 5001 mempty)
       <> showDefault
        )

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
