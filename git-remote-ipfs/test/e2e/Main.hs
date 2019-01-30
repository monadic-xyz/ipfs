{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import           Control.Applicative (liftA2)
import           Control.Exception.Safe (throwM, throwString)
import           Control.Lens (firstOf)
import           Control.Monad (void)
import           Data.Aeson.Lens (key, _String)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import           Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import           Data.Foldable (foldlM)
import qualified Data.Git.Monad as Git
import           Data.Git.Ref (SHA1)
import qualified Data.Git.Storage as Git (initRepo)
import           Data.Git.Types (GitTime(..))
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy(..))
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Stack (HasCallStack)
import qualified Network.HTTP.Client as Http
import           Network.IPFS.API (ApiV0KeyGen, ApiV0NamePublish)
import           Servant.API
import qualified Servant.Client as Servant
import           System.Entropy (getEntropy)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.FilePath (takeBaseName, takeDirectory, (</>))
import           System.Hourglass (timeCurrent, timezoneCurrent)
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process.Typed

import           Test.Tasty
import           Test.Tasty.HUnit

type Step = String -> IO ()

main :: IO ()
main = withSystemTempDirectory "git-remote-ipfs-e2e" $ \tmp -> defaultMain $
    testGroup "E2E Tests"
        [ testCaseSteps "Push, clone: IPFS"     $ testPushCloneSimple tmp
        , testCaseSteps "Push, clone: IPNS"     $ testPushCloneIPNS   tmp
        , testCaseSteps "Push, clone: LOB IPFS" $ testLargeObjects    tmp
        ]

testPushCloneSimple :: FilePath -> Step -> IO ()
testPushCloneSimple root step = runPushCloneTest PushCloneOpts
    { pcoRepo      = root </> "simple" </> ".git"
    , pcoClone     = root </> "simple-clone" </> ".git"
    , pcoRemoteUrl = "ipfs://"
    , pcoHistory   = simpleHistory
    , pcoLog       = step
    }

testPushCloneIPNS :: FilePath -> Step -> IO ()
testPushCloneIPNS root step = do
    let keyName = Text.pack (takeBaseName root) -- piggypack on randomness of tmp
    step "Creating IPNS name"
    ipnsName <- Text.unpack <$> ipfs (createIpnsName keyName)
    runPushCloneTest PushCloneOpts
        { pcoRepo      = root </> ipnsName </> ".git"
        , pcoClone     = root </> ipnsName ++ "-clone" </> ".git"
        , pcoRemoteUrl = "ipfs://ipns/" <> ipnsName
        , pcoHistory   = simpleHistory
        , pcoLog       = step
        }
  where
    ipfs m = servantEnv >>= Servant.runClientM m >>= either throwM pure

testLargeObjects :: FilePath -> Step -> IO ()
testLargeObjects root step = runPushCloneTest PushCloneOpts
    { pcoRepo      = root </> "lobs" </> ".git"
    , pcoClone     = root </> "lobs-clone" </> ".git"
    , pcoRemoteUrl = "ipfs://"
    , pcoHistory   = lobHistory
    , pcoLog       = step
    }

data PushCloneOpts = PushCloneOpts
    { pcoRepo      :: FilePath
    , pcoClone     :: FilePath
    , pcoRemoteUrl :: String
    , pcoHistory   :: FilePath -> IO ()
    , pcoLog       :: Step
    }

runPushCloneTest :: PushCloneOpts -> IO ()
runPushCloneTest PushCloneOpts{..} = do
    initRepo pcoLog pcoRepo pcoHistory

    url <- pushIpfs pcoLog pcoRepo pcoRemoteUrl
    cloneIpfs pcoLog (takeDirectory pcoClone) url

    assertSameRepos pcoLog pcoRepo pcoClone

initRepo :: HasCallStack => Step -> FilePath -> (FilePath -> IO ()) -> IO ()
initRepo step repo history = do
    step $ "Initializing repo at " <> repo
    let repo' = fromString repo
    Git.initRepo repo'
    (>>= either throwString pure) . Git.withRepo repo' $
        Git.headSet $ pure "master"
    history repo

pushIpfs :: Step -> FilePath -> String -> IO String
pushIpfs step repo url = do
    step $ "Pushing master to " <> url
    git_ repo ["remote", "add", "origin", url]
    git_ repo ["push", "--quiet", "origin", "master"]
    Text.unpack . Text.strip . decodeUtf8
        <$> git repo ["remote", "get-url", "origin"]

cloneIpfs :: Step -> FilePath -> String -> IO ()
cloneIpfs step repo url = do
    step $ "Cloning " <> url <> " to " <> repo
    git_ repo ["clone", "--quiet", url, repo]

assertSameRepos :: Step -> FilePath -> FilePath -> IO ()
assertSameRepos step src clone = do
    step $ "Fetching " <> src <> " to " <> clone
    git_ clone ["remote", "add", "src", src]
    git_ clone ["fetch", "--quiet", "src"]
    step "Comparing origin/master src/master"
    void $ gitAssert "Source and cloned repository differ"
        clone ["diff", "--quiet", "origin/master", "src/master"]

simpleHistory :: FilePath -> IO ()
simpleHistory repo =
    (>>= either throwString (const $ pure ())) . Git.withRepo (fromString repo) $ do
        initial <- mkCommit Nothing Nothing 0
        foldlM (\(bs, rev) i -> mkCommit bs rev i) initial [1..42]
  where
    mkCommit
        :: Maybe Lazy.ByteString
        -> Maybe String
        -> Int
        -> Git.GitM (Maybe Lazy.ByteString, Maybe String)
    mkCommit readme branch (show -> i) = do
        person  <- mkPerson <$> currentTimeGit
        (r, ()) <-
            Git.withNewCommit person branch $ do
                Git.setMessage $ "Commit " <> Bytes.pack i
                Git.setFile ["README"] $
                    fromMaybe mempty readme <> Lazy.pack i <> "\n"
        Git.branchWrite "master" r
        (,Just "master") <$>
            Git.withCommit ("master" :: String) (Git.getFile ["README"])

lobHistory :: FilePath -> IO ()
lobHistory repo = do
    blob <- getEntropy 4096000
    (>>= either throwString pure) . Git.withRepo (fromString repo) $ do
        person  <- mkPerson <$> currentTimeGit
        (r, ()) <-
            Git.withNewCommit person (Nothing :: Maybe (Git.Ref SHA1)) $ do
                Git.setMessage "Huuuuge"
                Git.setFile ["huge.file"] $ fromStrict blob
        Git.branchWrite "master" r

mkPerson :: GitTime -> Git.Person
mkPerson = Git.Person "LeBoeuf" "le@boe.uf"

currentTimeGit :: Git.GitM GitTime
currentTimeGit = Git.liftGit $ liftA2 GitTime timeCurrent timezoneCurrent

git :: FilePath -> [String] -> IO ByteString
git = gitAssert mempty

git_ :: FilePath -> [String] -> IO ()
git_ repo args = void $ git repo args

gitAssert :: String -> FilePath -> [String] -> IO ByteString
gitAssert msg repo args = do
    let cfg = proc "git" $ "--git-dir" : repo : args
    readProcess cfg >>= \case
        (ExitSuccess, out, _err) -> pure $ toStrict out
        (failure    , out, err ) -> assertFailure $ unlines
            [ msg
            , show failure
            , show cfg
            , show out
            , show err
            ]

type IPFS = ApiV0KeyGen :<|> ApiV0NamePublish

ipfsKeyGen :<|> ipfsNamePublish = Servant.client (Proxy @IPFS)

servantEnv :: IO Servant.ClientEnv
servantEnv = liftA2 Servant.mkClientEnv mgr base
  where
    mgr = Http.newManager Http.defaultManagerSettings
            { Http.managerResponseTimeout = Http.responseTimeoutNone }

    base =
        Servant.parseBaseUrl
            =<< fromMaybe "http://localhost:5001" <$> lookupEnv "IPFS_API_URL"

createIpnsName :: HasCallStack => Text -> Servant.ClientM Text
createIpnsName keyName = do
    keyId <-
        maybe (throwString "Missing key Id") pure
            . firstOf (key "Id" . _String)
            =<< ipfsKeyGen keyName (Just "ed25519") Nothing
    void $
        ipfsNamePublish ("/ipfs/" <> emptyRepo)
                        (Just True)  -- resolve
                        (Just "5m")  -- lifetime
                        Nothing      -- caching
                        (Just keyId) -- key
    pure keyId
  where
    emptyRepo = "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn"
