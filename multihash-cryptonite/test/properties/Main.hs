{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Control.Monad (unless)
import qualified Crypto.Hash as C
import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stderr, stdout)

import qualified Data.Multihash as Strict
import qualified Data.Multihash.Lazy as Lazy

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    ok <- props
    unless ok exitFailure

props :: IO Bool
props = checkParallel $$(discover)

prop_roundtripStrict :: Property
prop_roundtripStrict = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let
        hashed  = Strict.multihash C.Blake2b_256 bs
        bytes   = Strict.encodedBytes hashed
        decoded = Strict.fromDigest @C.Blake2b_256 <$> Strict.decodeDigest bytes
     in
        assert $ decoded == Right hashed

prop_roundtripBytesStrict :: Property
prop_roundtripBytesStrict = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let
        hashed  = Strict.multihash C.Blake2b_256 bs
        bytes   = Strict.encodedBytes hashed
        decoded = Strict.decode bytes
     in
        assert $ decoded == Right hashed

prop_roundtripLazy :: Property
prop_roundtripLazy = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let
        hashed  = Lazy.multihash C.Blake2b_256 bs
        bytes   = Lazy.encodedBytes hashed
        decoded = Lazy.fromDigest @C.Blake2b_256 <$> Lazy.decodeDigest bytes
     in
        assert $ decoded == Right hashed

prop_roundtripBytesLazy :: Property
prop_roundtripBytesLazy = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let
        hashed  = Lazy.multihash C.Blake2b_256 bs
        bytes   = Lazy.encodedBytes hashed
        decoded = Lazy.decode bytes
     in
        assert $ decoded == Right hashed

prop_algorithmMismatchStrict :: Property
prop_algorithmMismatchStrict = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let
        hashed  = Strict.multihash C.Blake2b_256 bs
        bytes   = Strict.encodedBytes hashed
        decoded = Strict.fromDigest @C.SHA256 <$> Strict.decodeDigest bytes
     in do
        annotate (render decoded)
        assert $ decoded == Left "Algorithm mismatch"

prop_algorithmMismatchLazy :: Property
prop_algorithmMismatchLazy = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let
        hashed  = Lazy.multihash C.Blake2b_256 bs
        bytes   = Lazy.encodedBytes hashed
        decoded = Lazy.fromDigest @C.SHA256 <$> Lazy.decodeDigest bytes
     in do
        annotate (render decoded)
        assert $ decoded == Left "Algorithm mismatch"

render :: Either String a -> String
render (Left  s) = "Left " <> s
render (Right _) = "Right <multihash>"
