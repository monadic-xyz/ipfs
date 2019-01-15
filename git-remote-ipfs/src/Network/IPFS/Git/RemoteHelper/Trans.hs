{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.IPFS.Git.RemoteHelper.Trans
    ( Logger (..)
    , defaultLogger

    , Env
    , newEnv
    , envVerbosity
    , envDryRun
    , envOptions
    , envClient
    , envIpfsRoot

    , RemoteHelper
    , RemoteHelperT
    , runRemoteHelper
    , runRemoteHelperT

    , RemoteHelperError
    , errError
    , errCallStack
    , throwRH
    , catchRH
    , mapError
    , liftEitherRH

    , logInfo
    , logDebug
    , logError

    , renderSourceLoc
    )
where

import           Control.Exception.Safe
import qualified Control.Lens as Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson.Lens as Lens
import           Data.Bifunctor (first)
import           Data.IORef (IORef, newIORef, readIORef)
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           GHC.Stack
                 ( CallStack
                 , HasCallStack
                 , callStack
                 , freezeCallStack
                 , getCallStack
                 , srcLocFile
                 , srcLocStartLine
                 )
import           System.IO (stderr)

import           Data.Git (Git)
import           Data.Git.Monad (GitMonad(..))
import           Data.Git.Ref (SHA1)
import           Data.Git.Storage (findRepo, openRepo)

import           Network.HTTP.Client
                 ( defaultManagerSettings
                 , managerResponseTimeout
                 , newManager
                 , responseTimeoutNone
                 )
import           Servant.Client.Streaming (mkClientEnv)
import qualified Servant.Client.Streaming as Servant

import           Data.IPLD.CID (CID, cidFromText)
import           Network.IPFS.API (ApiV0NameResolve)

import           Network.IPFS.Git.RemoteHelper.Internal (note)
import           Network.IPFS.Git.RemoteHelper.Options

data Logger = Logger
    { _logInfo  :: Text -> IO ()
    , _logDebug :: Text -> IO ()
    , _logError :: Text -> IO ()
    }

data Env = Env
    { envVerbosity :: IORef Word
    , envDryRun    :: IORef Bool
    , envOptions   :: Options
    , envLogger    :: Logger
    , envGit       :: Git SHA1
    , envClient    :: Servant.ClientEnv
    , envIpfsRoot  :: CID
    }

data RemoteHelperError a = RemoteHelperError
    { errCallStack :: CallStack
    , errError     :: a
    }

type RemoteHelper e = RemoteHelperT e IO

newtype RemoteHelperT e m a = RemoteHelperT
    { unRemoteHelperT :: ExceptT (RemoteHelperError e) (ReaderT Env m) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader Env
               , MonadError  (RemoteHelperError e)
               , MonadThrow
               , MonadCatch
               , MonadMask
               )

instance MonadIO m => GitMonad (RemoteHelperT e m) where
    getGit  = asks envGit
    liftGit = liftIO

remoteHelperError :: CallStack -> a -> RemoteHelperError a
remoteHelperError cs e = RemoteHelperError
    { errCallStack = cs
    , errError     = e
    }

throwRH :: (Monad m, HasCallStack) => e -> RemoteHelperT e m a
throwRH = throwError . remoteHelperError (freezeCallStack callStack)

catchRH
    :: Monad m
    => RemoteHelperT e m a
    -> (e -> RemoteHelperT e m a)
    -> RemoteHelperT e m a
catchRH ma f = catchError ma (f . errError)

mapError
    :: Monad m
    => (e -> e')
    -> RemoteHelperT e m a
    -> RemoteHelperT e' m a
mapError f (RemoteHelperT ma) =
    RemoteHelperT $ flip withExceptT ma $ \e ->
        e { errError = f (errError e) }

liftEitherRH :: (Monad m, HasCallStack) => Either e a -> RemoteHelperT e m a
liftEitherRH =
    liftEither . first (remoteHelperError (freezeCallStack callStack))


logInfo :: (HasCallStack, MonadIO m) => Text -> RemoteHelperT e m ()
logInfo msg = do
    out <- asks $ _logInfo . envLogger
    v   <- liftIO . readIORef =<< asks envVerbosity
    when (v > 0) $
        liftIO . out $ msg <> renderSourceLoc callStack

logDebug :: (HasCallStack, MonadIO m) => Text -> RemoteHelperT e m ()
logDebug msg = do
    out <- asks $ _logDebug . envLogger
    v   <- liftIO . readIORef =<< asks envVerbosity
    when (v > 1) $
        liftIO . out $ msg <> renderSourceLoc callStack

logError :: (HasCallStack, MonadIO m) => Text -> RemoteHelperT e m ()
logError msg = do
    out <- asks $ _logError . envLogger
    liftIO . out $ msg <> renderSourceLoc callStack

renderSourceLoc :: CallStack -> Text
renderSourceLoc cs =
    case getCallStack cs of
        ((_, loc) : _) ->
            " (" <> Text.pack (srcLocFile loc)
                 <> ":"
                 <> Text.pack (show (srcLocStartLine loc))
          <> ")"
        _ ->
            " (<unknown>)"

defaultLogger :: Logger
defaultLogger = Logger out out out
  where
    out = Text.hPutStrLn stderr

newEnv :: HasCallStack => Logger -> Options -> IO Env
newEnv envLogger envOptions = do
    envVerbosity <- newIORef 1
    envDryRun    <- newIORef False
    envGit       <- findRepo >>= openRepo
    envClient    <-
        flip mkClientEnv (optIpfsApiBaseUrl envOptions)
            <$> newManager defaultManagerSettings
                    { managerResponseTimeout = responseTimeoutNone }

    envIpfsRoot  <-
        case remoteUrlIpfsPath (optRemoteUrl envOptions) of
            IpfsPathIpfs cid  -> pure cid
            IpfsPathIpns name -> do
                _logInfo envLogger $ "Resolving IPNS name " <> name
                res <-
                    flip Servant.runClientM envClient $
                        ipfsNameResolve name
                                        (Just True)  -- recursive
                                        Nothing
                                        Nothing
                                        Nothing
                case res of
                    Left  e -> throwM e
                    Right v -> either throwString pure $ do
                        path <-
                            note "ipfsNameResolve: expected 'Path' key" $
                                Lens.firstOf (Lens.key "Path" . Lens._String) v
                        cidFromText
                            . fromMaybe path
                            $ Text.stripPrefix "/ipfs/" path

    pure Env {..}
  where
    ipfsNameResolve = Servant.client (Proxy @ApiV0NameResolve)

runRemoteHelperT
    :: Env
    -> RemoteHelperT e m a
    -> m (Either (RemoteHelperError e) a)
runRemoteHelperT r = flip runReaderT r . runExceptT . unRemoteHelperT

runRemoteHelper :: Env -> RemoteHelper e a -> IO (Either (RemoteHelperError e) a)
runRemoteHelper = runRemoteHelperT
