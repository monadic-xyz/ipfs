{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit (runConduit, (.|))
import           Data.Conduit.Binary (sinkHandle, sourceHandle)
import qualified Data.Conduit.Combinators as Conduit
import           Data.Foldable (for_)
import           Data.IORef (readIORef)
import           Data.Text (Text)
import qualified Data.Text as Text (pack)
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
            .| Conduit.map renderCommandResult
            .| Conduit.encodeUtf8
            .| trace "< "
            .| sinkHandle  stdout

    case res of
        Left  e -> do
            Text.hPutStrLn stderr $
                   renderError     (errError e)
                <> renderSourceLoc (errCallStack e)
            exitFailure

        Right _ -> exitSuccess
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
