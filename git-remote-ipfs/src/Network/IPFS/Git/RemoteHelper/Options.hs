{-# LANGUAGE OverloadedStrings #-}

module Network.IPFS.Git.RemoteHelper.Options where

import           Control.Applicative ((<|>))
import           Data.Bifunctor (first)
import           Data.List (stripPrefix)
import qualified Data.Text as Text
import           Options.Applicative
                 ( Parser
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

import           Servant.Client
                 ( BaseUrl(..)
                 , Scheme(..)
                 , parseBaseUrl
                 , showBaseUrl
                 )

import           Data.IPLD.CID (CID, cidFromText, cidToText)

data Options = Options
    { optRemoteName :: String
    , optCid        :: CID
    , optIpfsUrl    :: BaseUrl
    }

parseOptions :: Parser Options
parseOptions = Options
    <$> strArgument (metavar "REMOTE_NAME")
    <*> argument (eitherReader readCID) (metavar "URL")
    <*> option (eitherReader (first show . parseBaseUrl))
        ( long  "ipfs-url"
       <> help  "Base URL of the IPFS API"
       <> value (BaseUrl Http "localhost" 5001 mempty)
       <> showDefault
        )

prettyOptions :: Options -> String
prettyOptions Options{..} = unlines
    [ "Options"
    , "{ optRemoteName = \"" <> optRemoteName <> "\""
    , ", optCid        = "   <> show (cidToText optCid)
    , ", optIpfsUrl    = "   <> showBaseUrl optIpfsUrl
    , "}"
    ]

readCID :: String -> Either String CID
readCID s =
    maybe (Left "Invalid URI scheme") go $
        stripPrefix "ipfs://" s <|> stripPrefix "ipld://" s
  where
    go :: String -> Either String CID
    go = cidFromText . \case
        [] -> emptyRepo
        xs -> Text.pack xs

    emptyRepo = "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn"
