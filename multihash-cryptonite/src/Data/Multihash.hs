{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

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
--         input = "multihash" :: ByteString
--         multihash' base algo = convertToBase base . encodedBytes . multihash algo
--      in
--         C8.hPutStr stdout $ C8.unlines
--             [ multihash' Base16 C.SHA1   input
--             , multihash' Base32 C.SHA1   input
--             , multihash' Base16 C.SHA256 input
--             , multihash' Base32 C.SHA256 input
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
    , decodeBytes

    -- * Compact representation
    , CompactMultihash
    , compact
    , expand

    -- * Re-exports
    , Multihashable
    , fromCryptonite
    , toCode
    , fromCode
    )
where

import           Data.Multihash.Internal

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


-- | A multihash-encoded strict 'ByteString'.
newtype Multihash = Multihash ByteString
    deriving Eq

-- | A 'Multihash' backed by a 'ShortByteString'.
--
-- This is useful when holding many 'Multihash'es in memory, due to lower memory
-- overhead and less heap fragmentation. See the documentation for
-- 'ShortByteString' for details.
newtype CompactMultihash = Compact ShortByteString
    deriving Eq

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

-- | Decode a 'C.Digest' from a multihash-encoded 'ByteString'.
decodeBytes
    :: forall a. Multihashable a
    => ByteString
    -> Either String (C.Digest a)
decodeBytes = bimap _3 _3 . Binary.runGetOrFail getMultihash . LBS.fromStrict
  where
    _3 (_,_,x) = x

-- | Convert a 'Multihash' to a compact representation.
compact :: Multihash -> CompactMultihash
compact = coerce . toShort . coerce
{-# INLINE compact #-}

-- | Convert a 'CompactMultihash' to the regular representation.
expand :: CompactMultihash -> Multihash
expand = coerce . fromShort . coerce
{-# INLINE expand #-}
