{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Network.IPFS.Git.RemoteHelper.Trans
    ( Logger (..)
    , defaultLogger

    , Env
    , newEnv
    , envVerbosity
    , envDryRun
    , envOptions
    , envIpfsOptions
    , envGit
    , envClient
    , envIpfsRoot
    , envLobs

    , RemoteHelper
    , RemoteHelperT
    , runRemoteHelper
    , runRemoteHelperT

    , DisplayError (..)

    , RemoteHelperError
    , errError
    , errCallStack
    , throwRH
    , catchRH
    , mapError
    , liftEitherRH

    , concurrently
    , concurrently_
    , forConcurrently_
    , forConcurrently

    , logInfo
    , logDebug
    , logError

    , renderSourceLoc
    )
where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newMVar, withMVar)
import           Control.Exception.Safe
import qualified Control.Lens as Lens
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Control.Monad.Reader
import qualified Data.Aeson.Lens as Lens
import           Data.Bifunctor (first)
import           Data.HashMap.Strict (HashMap)
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
import qualified Data.Git.Repository as Git (configGet)

import           Network.HTTP.Client
                 ( defaultManagerSettings
                 , managerResponseTimeout
                 , newManager
                 , responseTimeoutNone
                 )
import           Servant.Client.Streaming (mkClientEnv)
import qualified Servant.Client.Streaming as Servant

import           Data.IPLD.CID (CID, cidFromText, cidToText)
import           Network.IPFS.API (ApiV0NameResolve)

import           Network.IPFS.Git.RemoteHelper.Internal (note)
import           Network.IPFS.Git.RemoteHelper.Options

data Logger = Logger
    { _logInfo  :: Text -> IO ()
    , _logDebug :: Text -> IO ()
    , _logError :: Text -> IO ()
    }

data Env = Env
    { envVerbosity   :: IORef Word
    , envDryRun      :: IORef Bool
    , envOptions     :: Options
    , envIpfsOptions :: IpfsOptions
    , envLogger      :: Logger
    , envGit         :: Git SHA1
    , envGitMutex    :: MVar ()
    , envClient      :: Servant.ClientEnv
    , envIpfsRoot    :: CID
    , envLobs        :: MVar (Maybe (HashMap CID CID))
    }

class DisplayError a where
    displayError :: a -> Text

data RemoteHelperError a = RemoteHelperError
    { errCallStack :: CallStack
    , errError     :: a
    } deriving Show

instance
    (Show a, Typeable a, DisplayError a)
    => Exception (RemoteHelperError a)
  where
    displayException = Text.unpack . displayError

instance DisplayError a => DisplayError (RemoteHelperError a) where
    displayError e =
        displayError (errError e) <> " " <> renderSourceLoc (errCallStack e)

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

instance MonadTrans (RemoteHelperT e) where
    lift = RemoteHelperT . lift . lift
    {-# INLINE lift #-}

instance PrimMonad m => PrimMonad (RemoteHelperT e m) where
    type PrimState (RemoteHelperT e m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}

instance MonadIO m => GitMonad (RemoteHelperT e m) where
    getGit    = asks envGit
    liftGit f = do
        lck <- asks envGitMutex
        liftIO . withMVar lck . const $ f

    {-# INLINE getGit  #-}
    {-# INLINE liftGit #-}

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

concurrently
    :: (Show e, Typeable e, DisplayError e)
    => RemoteHelperT e IO a
    -> RemoteHelperT e IO b
    -> RemoteHelperT e IO (a, b)
concurrently left right = do
    env <- ask
    liftIO $ Async.concurrently
        (either throwM pure =<< runRemoteHelperT env left)
        (either throwM pure =<< runRemoteHelperT env right)

concurrently_
    :: (Show e, Typeable e, DisplayError e)
    => RemoteHelperT e IO a
    -> RemoteHelperT e IO b
    -> RemoteHelperT e IO ()
concurrently_ left right = do
    env <- ask
    liftIO $ Async.concurrently_
        (either throwM pure =<< runRemoteHelperT env left)
        (either throwM pure =<< runRemoteHelperT env right)

forConcurrently_
    :: ( Foldable     t
       , Show         e
       , Typeable     e
       , DisplayError e
       )
    => t a
    -> (a -> RemoteHelperT e IO b)
    -> RemoteHelperT e IO ()
forConcurrently_ xs f = do
    env <- ask
    liftIO $
        Async.forConcurrently_ xs $ \x ->
            either throwM pure =<< runRemoteHelperT env (f x)

forConcurrently
    :: ( Traversable  t
       , Show         e
       , Typeable     e
       , DisplayError e
       )
    => t a
    -> (a -> RemoteHelperT e IO b)
    -> RemoteHelperT e IO (t b)
forConcurrently xs f = do
    env <- ask
    liftIO $
        Async.forConcurrently xs $ \x ->
            either throwM pure =<< runRemoteHelperT env (f x)

logInfo :: MonadIO m => Text -> RemoteHelperT e m ()
logInfo msg = do
    out <- asks $ _logInfo . envLogger
    v   <- liftIO . readIORef =<< asks envVerbosity
    when (v > 0) $
        liftIO $ out msg

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

newEnv :: HasCallStack => Logger -> Options -> IpfsOptions -> IO Env
newEnv envLogger envOptions envIpfsOptions = do
    envVerbosity <- newIORef 1
    envDryRun    <- newIORef False
    envGit       <- findRepo >>= openRepo
    envGitMutex  <- newMVar ()
    envLobs      <- newMVar Nothing
    envClient    <-
        flip mkClientEnv (ipfsApiUrl envIpfsOptions)
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
                    Left  e
                      | isCouldNotResolve e -> do

                        lastSeen <-
                            Git.configGet envGit
                                         ("remote." <> optRemoteName envOptions)
                                         "lastseencid"
                        case lastSeen of
                            Nothing -> throwString $
                                  "Could not resolve repo IPNS name "
                               <> Text.unpack name
                               <> " and no fallback found."
                            Just ls -> do
                                root <-
                                    either throwString pure $
                                        cidFromText (Text.pack ls)
                                _logInfo envLogger $
                                       "Warning: Could not resolve repo IPNS name "
                                    <> name
                                    <> ", falling back to last seen CID: "
                                    <> cidToText root
                                pure root

                      | otherwise -> throwM e

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

    isCouldNotResolve = \case
#if MIN_VERSION_servant_client(0,16,0)
        Servant.FailureResponse _ res ->
#else
        Servant.FailureResponse   res ->
#endif
            case Lens.firstOf (Lens.key "Message") (Servant.responseBody res) of
                Just "could not resolve name" -> True
                _                             -> False
        _ -> False


runRemoteHelperT
    :: Env
    -> RemoteHelperT e m a
    -> m (Either (RemoteHelperError e) a)
runRemoteHelperT r = flip runReaderT r . runExceptT . unRemoteHelperT

runRemoteHelper :: Env -> RemoteHelper e a -> IO (Either (RemoteHelperError e) a)
runRemoteHelper = runRemoteHelperT
