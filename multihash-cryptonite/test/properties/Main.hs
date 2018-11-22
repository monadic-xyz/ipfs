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
        decoded = Strict.fromDigest @C.Blake2b_256 <$> Strict.decodeBytes bytes
     in
        assert $ decoded == Right hashed

prop_roundtripLazy :: Property
prop_roundtripLazy = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let
        hashed  = Lazy.multihash C.Blake2b_256 bs
        bytes   = Lazy.encodedBytes hashed
        decoded = Lazy.fromDigest @C.Blake2b_256 <$> Lazy.decodeBytes bytes
     in
        assert $ decoded == Right hashed
