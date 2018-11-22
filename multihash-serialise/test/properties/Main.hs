{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Monad (unless)
import qualified Crypto.Hash as C
import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stderr, stdout)

import qualified Codec.Serialise as CBOR
import           Codec.Serialise.Multihash (Multihashable)
import qualified Codec.Serialise.Multihash as Multihash

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

newtype Digest' a = Digest' { unDigest' :: C.Digest a }
    deriving (Eq, Show)

instance Multihashable a => CBOR.Serialise (Digest' a) where
    encode = Multihash.encode . unDigest'
    decode = Digest' <$> Multihash.decode

prop_roundtrip :: Property
prop_roundtrip = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let digest = Digest' $ C.hashWith C.Blake2b_256 bs
    tripping digest CBOR.serialise CBOR.deserialiseOrFail
