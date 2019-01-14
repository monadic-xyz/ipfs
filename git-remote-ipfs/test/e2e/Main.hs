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
import           Data.Time (getZonedTime)
import           GHC.Stack (HasCallStack)
import           Git
import           Git.Libgit2 (LgRepo, lgFactory)
import qualified Network.HTTP.Client as Http
import           Network.IPFS.API (ApiV0KeyGen, ApiV0NamePublish)
import           Servant.API
import qualified Servant.Client as Servant
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.FilePath (takeBaseName, takeDirectory, (</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process.Typed

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = withSystemTempDirectory "git-remote-ipfs-e2e" $ \tmp -> defaultMain $
    testGroup "E2E Tests"
        [ testCase "Simple push, clone works" $ testPushCloneSimple tmp
        , testCase "IPNS push, clone works"   $ testPushCloneIPNS   tmp
        ]

testPushCloneSimple :: FilePath -> IO ()
testPushCloneSimple root = do
    let repoPath  = root </> "simple" </> ".git"
    let clonePath = root </> "simple-clone" </> ".git"

    initRepo repoPath simpleHistory

    url <- do
        git_ repoPath ["remote", "add", "ipfs", "ipfs://"]
        git_ repoPath ["push", "--quiet", "ipfs", "master"]
        Text.strip . decodeUtf8 <$> git repoPath ["remote", "get-url", "ipfs"]

    git_ clonePath ["clone", "--quiet", Text.unpack url, takeDirectory clonePath]

    assertSameRepos repoPath clonePath

testPushCloneIPNS :: HasCallStack => FilePath -> IO ()
testPushCloneIPNS root = do
    let keyName = Text.pack (takeBaseName root) -- piggypack on randomness of tmp
    ipnsName <- Text.unpack <$> ipfs (createIpnsName keyName)

    let repoPath  = root </> ipnsName </> ".git"
    let clonePath = root </> ipnsName ++ "-clone" </> ".git"
    let remoteUrl = "ipfs://ipns/" <> ipnsName

    initRepo repoPath simpleHistory

    git_ repoPath ["remote", "add", "ipns", remoteUrl]
    git_ repoPath ["push", "--quiet", "ipns", "master"]

    git_ clonePath ["clone", "--quiet", remoteUrl, takeDirectory clonePath]

    assertSameRepos repoPath clonePath
  where
    ipfs m = servantEnv >>= Servant.runClientM m >>= either throwM pure

    createIpnsName :: Text -> Servant.ClientM Text
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

    emptyRepo = "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn"

initRepo
    :: HasCallStack
    => FilePath
    -> ReaderT LgRepo IO (Maybe (Commit LgRepo))
    -> IO ()
initRepo path history =
    withRepository' lgFactory (ropts path True) $
        maybe (throwString "headless history")
              (updateReference "refs/heads/master" . RefObj . untag . commitOid)
            =<< history

assertSameRepos :: FilePath -> FilePath -> IO ()
assertSameRepos src clone = do
    git_ clone ["remote", "add", "src", src]
    git_ clone ["fetch", "--quiet", "src"]
    void $ gitAssert "Source and cloned repository differ"
        clone ["diff", "--quiet", "origin/master", "src/master"]

simpleHistory :: ReaderT LgRepo IO (Maybe (Commit LgRepo))
simpleHistory =
    foldlM (\parent -> fmap Just . mkCommit parent) Nothing ['a'..'z']
  where
    sig t = Signature
        { signatureName  = "LeBoeuf"
        , signatureEmail = "le@boe.uf"
        , signatureWhen  = t
        }

    mkCommit parent c = do
        blob <- createBlobUtf8 $ "Blob " <> Text.singleton c
        tree <-
            maybe createTree (mutateTreeOid . commitTree) parent $
                putBlob ("blobs/" <> Bytes.singleton c) blob
        sig' <- sig <$> liftIO getZonedTime
        createCommit (maybeToList . fmap commitOid $ parent)
                     tree
                     sig'
                     sig'
                     ("Commit " <> Text.singleton c)
                     (Just "HEAD")

ropts :: FilePath -> Bool -> RepositoryOptions
ropts p autoCreate = RepositoryOptions
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
