{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import           Control.Applicative (liftA2)
import           Control.Exception.Safe (throwM, throwString)
import           Control.Lens (firstOf)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson.Lens (key, _String)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable (foldlM)
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Proxy (Proxy(..))
import           Data.Tagged (untag)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time (ZonedTime, getZonedTime)
import           GHC.Stack (HasCallStack)
import           Git
import           Git.Libgit2 (LgRepo, lgFactory)
import qualified Network.HTTP.Client as Http
import           Network.IPFS.API (ApiV0KeyGen, ApiV0NamePublish)
import           Servant.API
import qualified Servant.Client as Servant
import           System.Entropy (getEntropy)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.FilePath (takeBaseName, takeDirectory, (</>))
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
    , pcoHistory   :: ReaderT LgRepo IO (Commit LgRepo)
    , pcoLog       :: Step
    }

runPushCloneTest :: PushCloneOpts -> IO ()
runPushCloneTest PushCloneOpts{..} = do
    initRepo pcoLog pcoRepo pcoHistory
    url <- pushIpfs pcoLog pcoRepo pcoRemoteUrl
    cloneIpfs pcoLog (takeDirectory pcoClone) url
    assertSameRepos pcoLog pcoRepo pcoClone

initRepo :: Step -> FilePath -> ReaderT LgRepo IO (Commit LgRepo) -> IO ()
initRepo step path history = do
    step $ "Initializing repo at " <> path
    withRepository' lgFactory (mkROpts path True) $
        history >>=
            updateReference "refs/heads/master" . RefObj . untag . commitOid

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

simpleHistory :: ReaderT LgRepo IO (Commit LgRepo)
simpleHistory =
    maybe (throwString "headless history") pure =<<
        foldlM (\parent -> fmap Just . mkCommit parent) Nothing ['a'..'z']
  where
    mkCommit parent c = do
        blob <- createBlobUtf8 $ "Blob " <> Text.singleton c
        tree <-
            maybe createTree (mutateTreeOid . commitTree) parent $
                putBlob ("blobs/" <> Bytes.singleton c) blob
        sig' <- mkSig <$> liftIO getZonedTime
        createCommit (maybeToList . fmap commitOid $ parent)
                     tree
                     sig'
                     sig'
                     ("Commit " <> Text.singleton c)
                     (Just "HEAD")

lobHistory :: ReaderT LgRepo IO (Commit LgRepo)
lobHistory = do
    blob <- createBlob . BlobString =<< liftIO (getEntropy 4096000)
    tree <- createTree $ putBlob "binarylargeobject" blob
    sig' <- mkSig <$> liftIO getZonedTime
    createCommit [] tree sig' sig' "A large bunch of bytes" (Just "HEAD")

mkSig :: ZonedTime -> Signature
mkSig t = Signature
    { signatureName  = "LeBoeuf"
    , signatureEmail = "le@boe.uf"
    , signatureWhen  = t
    }

mkROpts :: FilePath -> Bool -> RepositoryOptions
mkROpts p autoCreate = RepositoryOptions
    { repoPath       = p
    , repoWorkingDir = Nothing
    , repoIsBare     = False
    , repoAutoCreate = autoCreate
    }

git :: FilePath -> [String] -> IO ByteString
git = gitAssert mempty

git_ :: FilePath -> [String] -> IO ()
git_ repo args = void $ git repo args

gitAssert :: String -> FilePath -> [String] -> IO ByteString
gitAssert msg repo args =
    let
        cfg = proc "git" ("--git-dir" : repo : args)
     in
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
