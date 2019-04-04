{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.Monad (unless)
import qualified Crypto.Hash as C
import           Data.ByteString (ByteString)
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (for_)
import           Data.Functor.Identity (Identity)
import           Data.List (isInfixOf)
import           Data.Text.Encoding (decodeUtf8)
import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stderr, stdout)

import           Data.ByteString.Multibase (fromMultibase)
import qualified Data.ByteString.Multibase as Multibase
import           Data.IPLD.CID
                 ( CID
                 , buildCid
                 , cidFromText
                 , cidToText
                 , newCidV0
                 , newCidV1
                 )
import qualified Data.IPLD.CID as CID
import           Data.Multihash (Multihash, Multihashable)
import qualified Data.Multihash as Multihash
import           Data.Multihash.Internal (HashAlgorithm(..))

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = do
    for_ [stdout, stderr] $ flip hSetBuffering LineBuffering

    ok <- props
    unless ok exitFailure

props :: IO Bool
props = checkParallel $$(discover)

prop_roundtripBytes :: Property
prop_roundtripBytes = property $ do
    cid <- forAll genCID
    tripping cid (Builder.toLazyByteString . buildCid) (CID.decodeCid . LBS.toStrict)

prop_roundtripText :: Property
prop_roundtripText = property $ do
    cid <- forAll genCID
    tripping cid cidToText cidFromText

-- | "If the first decoded byte is 0x12, return an error. CIDv0 CIDs may not be
-- multibase encoded and there will be no CIDv18 (0x12 = 18) to prevent
-- ambiguity with decoded CIDv0s."
--
-- It seems like the only way this can happen is to pass a 'C.SHA256'
-- 'Multihash' (or an arbitrary byte sequence which happens to start with 0x12),
-- encoded at some base /= 'Base58btc'. ie. the multibase encoding has to be
-- valid.
prop_ambiguousVersion :: Property
prop_ambiguousVersion = property $ do
    hash <- forAllWith (show . Multihash.encodedBytes) (genMultihash C.SHA256)
    let
        encoded = Multibase.encode (BaseN.encodeAtBase BaseN.Base32 . Multihash.encodedBytes $ hash)
        notACid = decodeUtf8 $ fromMultibase encoded
        fromTxt = cidFromText notACid
     in do
        annotate (show $ fromMultibase encoded)
        annotateShow notACid
        annotateShow fromTxt
        case fromTxt of
            Left e | "reserved" `isInfixOf` e -> success
            _                                 -> failure
-- Generators ------------------------------------------------------------------

genCID :: GenT Identity CID
genCID = Gen.choice [v0, v1]
  where
    v0 = newCidV0 <$> genDigest
    v1 = do
        codec <- genCodec
        algo  <- genHashAlgorithm
        case algo of
            Blake2s_160 -> newCidV1 codec <$> genDigest @C.Blake2s_160
            Blake2s_224 -> newCidV1 codec <$> genDigest @C.Blake2s_224
            Blake2s_256 -> newCidV1 codec <$> genDigest @C.Blake2s_256
            Blake2b_160 -> newCidV1 codec <$> genDigest @C.Blake2b_160
            Blake2b_224 -> newCidV1 codec <$> genDigest @C.Blake2b_224
            Blake2b_256 -> newCidV1 codec <$> genDigest @C.Blake2b_256
            Blake2b_384 -> newCidV1 codec <$> genDigest @C.Blake2b_384
            Blake2b_512 -> newCidV1 codec <$> genDigest @C.Blake2b_512
            MD4         -> newCidV1 codec <$> genDigest @C.MD4
            MD5         -> newCidV1 codec <$> genDigest @C.MD5
            SHA1        -> newCidV1 codec <$> genDigest @C.SHA1
            SHA256      -> newCidV1 codec <$> genDigest @C.SHA256
            SHA512      -> newCidV1 codec <$> genDigest @C.SHA512
            Keccak_224  -> newCidV1 codec <$> genDigest @C.Keccak_224
            Keccak_256  -> newCidV1 codec <$> genDigest @C.Keccak_256
            Keccak_384  -> newCidV1 codec <$> genDigest @C.Keccak_384
            Keccak_512  -> newCidV1 codec <$> genDigest @C.Keccak_512
            SHA3_224    -> newCidV1 codec <$> genDigest @C.SHA3_224
            SHA3_256    -> newCidV1 codec <$> genDigest @C.SHA3_256
            SHA3_384    -> newCidV1 codec <$> genDigest @C.SHA3_384
            SHA3_512    -> newCidV1 codec <$> genDigest @C.SHA3_512

genMultihash :: forall a. Multihashable a => a -> GenT Identity Multihash
genMultihash _ = Multihash.fromDigest <$> genDigest @a

genDigest :: Multihashable a => GenT Identity (C.Digest a)
genDigest = C.hash <$> genBytes

genHashAlgorithm :: GenT Identity Multihash.HashAlgorithm
genHashAlgorithm = Gen.prune Gen.enumBounded

genBytes :: GenT Identity ByteString
genBytes = Gen.prune $ Gen.bytes (Range.singleton 255)

genCodec :: GenT Identity CID.Codec
genCodec = Gen.element [CID.Raw, CID.DagProtobuf, CID.DagCbor, CID.GitRaw]
