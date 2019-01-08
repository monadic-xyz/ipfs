{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Attoparsec.ByteString (parseOnly)
import           Data.Bool (bool)
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit (ZipSink(..), runConduit, (.|))
import           Data.Conduit.Binary (sinkHandle, sourceHandle)
import qualified Data.Conduit.Combinators as Conduit
import           Data.Foldable (for_)
import           Data.IORef (readIORef)
import           Data.Text (Text)
import qualified Data.Text as Text (pack, unpack)
import qualified Data.Text.IO as Text (hPutStrLn)
import           Options.Applicative
import           System.Exit (exitFailure, exitSuccess)
import           System.IO
                 ( BufferMode(LineBuffering)
                 , hSetBuffering
                 , stderr
                 , stdin
                 , stdout
                 )
import           System.Process.Typed (runProcess_, shell)

import           Data.IPLD.CID (cidToText)

import           Network.IPFS.Git.RemoteHelper
import           Network.IPFS.Git.RemoteHelper.Command
import           Network.IPFS.Git.RemoteHelper.Options
import           Network.IPFS.Git.RemoteHelper.Trans

data Error
    = ParseError String
    | ProcError  ProcessError

renderError :: Error -> Text
renderError = \case
    ParseError e -> "Command failed to parse: " <> Text.pack e
    ProcError  e -> renderProcessError e

main :: IO ()
main = do
    for_ [stdin, stdout, stderr] $ flip hSetBuffering LineBuffering

    env <- execParser optInfo >>= newEnv defaultLogger
    res <-
        runRemoteHelper env . runConduit $
               sourceHandle stdin
            .| Conduit.linesUnboundedAscii
            .| Conduit.filter (/= "") -- XXX: batching not supported yet
            .| trace "> "
            .| Conduit.mapM run
            .| tee ( Conduit.map renderCommandResult
                  .| Conduit.encodeUtf8
                  .| trace "< "
                  .| sinkHandle  stdout
                   )
    err <-
        case res of
            Left  e  -> do
                Text.hPutStrLn stderr $
                       renderError     (errError e)
                    <> renderSourceLoc (errCallStack e)
                pure True

            Right (Just (PushResult (PushOk _ cid))) ->
                let
                    remoteName = Text.pack $ optRemoteName (envOptions env)
                    configKey  = "remote." <> remoteName <> ".url"
                    remoteUrl  = "ipfs://" <> cidToText cid
                 in do
                    Text.hPutStrLn stderr $
                        "Updating remote " <> remoteName <> " to " <> remoteUrl
                    runProcess_ . shell . Text.unpack $
                        "git config " <> configKey <> " " <> remoteUrl
                    pure False

            Right Nothing -> True <$ Text.hPutStrLn stderr "Empty result"
            Right _       -> pure False

    bool exitSuccess exitFailure err
  where
    optInfo = info (helper <*> parseOptions) fullDesc

    trace prefix = Conduit.mapM $ \x -> do
        v <- liftIO . readIORef =<< asks envVerbosity
        when (v > 1) $
            liftIO (C8.hPutStr stderr prefix *> C8.hPutStrLn stderr x)
        pure x

    run bs = do
        cmd <- either (throwRH . ParseError) pure $ parseOnly parseCommand bs
        mapError ProcError $ processCommand cmd

    tee f = getZipSink $ ZipSink Conduit.last <* ZipSink f
