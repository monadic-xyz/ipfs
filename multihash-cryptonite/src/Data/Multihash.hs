{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Copyright   : 2018 Monadic GmbH
-- License     : BSD3
-- Maintainer  : kim@monadic.xyz, team@monadic.xyz
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- <https://github.com/multiformats/multihash multihash> encoding of 'Crypto.Hash.Digest's.
--
-- Example:
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Crypto.Hash as C
-- >>> import Data.ByteArray.Encoding (Base(..), convertToBase)
-- >>> import Data.ByteString (ByteString)
-- >>> import qualified Data.ByteString.Char8 as C8
-- >>> import System.IO (stdout)
-- >>> :{
--     let
--         input :: ByteString
--         input = "multihash"
--         atBase :: Base -> Multihash -> ByteString
--         atBase base = convertToBase base . encodedBytes
--      in
--         C8.hPutStr stdout $ C8.unlines
--             [ atBase Base16 $ multihash C.SHA1   input
--             , atBase Base32 $ multihash C.SHA1   input
--             , atBase Base16 $ multihash C.SHA256 input
--             , atBase Base32 $ multihash C.SHA256 input
--             ]
-- :}
-- 111488c2f11fb2ce392acb5b2986e640211c4690073e
-- CEKIRQXRD6ZM4OJKZNNSTBXGIAQRYRUQA47A====
-- 12209cbc07c3f991725836a3aa2a581ca2029198aa420b9d99bc0e131d9f3e2cbe47
-- CIQJZPAHYP4ZC4SYG2R2UKSYDSRAFEMYVJBAXHMZXQHBGHM7HYWL4RY=
--
module Data.Multihash
    ( Multihash
    , fromDigest
    , encodedBytes
    , multihash
    , decode
    , decodeDigest
    , getMultihash

    -- * Compact representation
    , CompactMultihash
    , compact
    , expand

    -- * Re-exports
    , HashAlgorithm
    , Multihashable
    , fromCryptonite
    , toCode
    , fromCode
    , digestSize
    )
where

import           Data.Multihash.Internal

import           Control.DeepSeq (NFData)
import qualified Crypto.Hash as C
import           Data.Bifunctor (bimap)
import qualified Data.Binary.Get as Binary
import           Data.Binary.VarInt (buildVarInt)
import           Data.ByteArray (ByteArrayAccess, convert)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Short (ShortByteString, fromShort, toShort)
import           Data.Coerce (coerce)
import           Data.Hashable (Hashable)

-- | A multihash-encoded strict 'ByteString'.
newtype Multihash = Multihash ByteString
    deriving (Eq, Ord, Hashable, NFData)

-- | A 'Multihash' backed by a 'ShortByteString'.
--
-- This is useful when holding many 'Multihash'es in memory, due to lower memory
-- overhead and less heap fragmentation. See the documentation for
-- 'ShortByteString' for details.
newtype CompactMultihash = Compact ShortByteString
    deriving (Eq, Ord, Hashable, NFData)

-- | Encode a 'C.Digest' as a 'Multihash'.
fromDigest :: forall a. Multihashable a => C.Digest a -> Multihash
fromDigest dig =
    Multihash
        . LBS.toStrict . Builder.toLazyByteString
        $ code <> len <> Builder.byteString bytes
  where
    code  = buildVarInt . toCode $ fromCryptonite dig
    bytes = convert dig
    len   = buildVarInt $ BS.length bytes

-- | Extract the raw, multihash-encoded bytes of a 'Multihash'.
encodedBytes :: Multihash -> ByteString
encodedBytes = coerce
{-# INLINE encodedBytes #-}

-- | Hash a value to a 'Multihash'
multihash :: (ByteArrayAccess ba, Multihashable a) => a -> ba -> Multihash
multihash rithm = fromDigest . C.hashWith rithm

-- | Decode a 'Multihash' from a 'ByteString'.
decode :: ByteString -> Either String Multihash
decode =
    bimap _3 _3
        . Binary.runGetOrFail getMultihash
        . LBS.fromStrict

-- | Decode a 'C.Digest' from a multihash-encoded 'ByteString'.
decodeDigest
    :: forall a. Multihashable a
    => ByteString
    -> Either String (C.Digest a)
decodeDigest =
    bimap _3 _3
        . Binary.runGetOrFail getMultihashedDigest
        . LBS.fromStrict

getMultihash :: Binary.Get Multihash
getMultihash = do
    algo <- Binary.lookAhead getHashAlgorithm
    case algo of
        Blake2s_160 -> fromDigest <$> getMultihashedDigest @C.Blake2s_160
        Blake2s_224 -> fromDigest <$> getMultihashedDigest @C.Blake2s_224
        Blake2s_256 -> fromDigest <$> getMultihashedDigest @C.Blake2s_256
        Blake2b_160 -> fromDigest <$> getMultihashedDigest @C.Blake2b_160
        Blake2b_224 -> fromDigest <$> getMultihashedDigest @C.Blake2b_224
        Blake2b_256 -> fromDigest <$> getMultihashedDigest @C.Blake2b_256
        Blake2b_384 -> fromDigest <$> getMultihashedDigest @C.Blake2b_384
        Blake2b_512 -> fromDigest <$> getMultihashedDigest @C.Blake2b_512
        MD4         -> fromDigest <$> getMultihashedDigest @C.MD4
        MD5         -> fromDigest <$> getMultihashedDigest @C.MD5
        SHA1        -> fromDigest <$> getMultihashedDigest @C.SHA1
        SHA256      -> fromDigest <$> getMultihashedDigest @C.SHA256
        SHA512      -> fromDigest <$> getMultihashedDigest @C.SHA512
        Keccak_224  -> fromDigest <$> getMultihashedDigest @C.Keccak_224
        Keccak_256  -> fromDigest <$> getMultihashedDigest @C.Keccak_256
        Keccak_384  -> fromDigest <$> getMultihashedDigest @C.Keccak_384
        Keccak_512  -> fromDigest <$> getMultihashedDigest @C.Keccak_512
        SHA3_224    -> fromDigest <$> getMultihashedDigest @C.SHA3_224
        SHA3_256    -> fromDigest <$> getMultihashedDigest @C.SHA3_256
        SHA3_384    -> fromDigest <$> getMultihashedDigest @C.SHA3_384
        SHA3_512    -> fromDigest <$> getMultihashedDigest @C.SHA3_512

-- | Convert a 'Multihash' to a compact representation.
compact :: Multihash -> CompactMultihash
compact = coerce . toShort . coerce
{-# INLINE compact #-}

-- | Convert a 'CompactMultihash' to the regular representation.
expand :: CompactMultihash -> Multihash
expand = coerce . fromShort . coerce
{-# INLINE expand #-}
