{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- |
-- Copyright   : 2018 Monadic GmbH
-- License     : BSD3
-- Maintainer  : kim@monadic.xyz, team@monadic.xyz
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Like "Data.Multihash", but using a lazy 'ByteString' representation.
--
-- Example:
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Crypto.Hash as C
-- >>> import Data.ByteArray.Encoding (Base(..), convertToBase)
-- >>> import qualified Data.ByteString as Strict
-- >>> import qualified Data.ByteString.Lazy as Lazy
-- >>> import Data.ByteString.Lazy (toStrict)
-- >>> import qualified Data.ByteString.Char8 as C8
-- >>> import System.IO (stdout)
-- >>> :{
--     let
--         input :: Strict.ByteString
--         input = "multihash"
--         atBase :: Base -> Multihash -> Strict.ByteString
--         atBase base = convertToBase base . toStrict . encodedBytes
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
module Data.Multihash.Lazy
    ( Multihash
    , fromDigest
    , encodedBytes
    , multihash
    , decode
    , decodeDigest
    , getMultihash

    -- * Re-exports
    , Multihashable
    )
where

import           Data.Multihash.Internal

import           Control.DeepSeq (NFData)
import qualified Crypto.Hash as C
import           Data.Bifunctor (bimap)
import qualified Data.Binary.Get as Binary
import           Data.Binary.VarInt (buildVarInt)
import           Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Lazy (ByteString)
import           Data.Coerce (coerce)
import           Data.Hashable (Hashable)

-- | A multihash-encoded lazy 'ByteString'
newtype Multihash = Multihash ByteString
    deriving (Eq, Ord, Hashable, NFData)

-- | Encode a 'C.Digest' as a 'Multihash'.
fromDigest :: forall a. Multihashable a => C.Digest a -> Multihash
fromDigest dig =
    Multihash
        . Builder.toLazyByteString
        $ code <> len <> Builder.byteString bytes
  where
    code  = buildVarInt . toCode $ fromCryptonite dig
    bytes = convert dig
    len   = buildVarInt $ Strict.length bytes

-- | Extract the raw, multihash-encoded bytes of a 'Multihash'.
encodedBytes :: Multihash -> ByteString
encodedBytes = coerce
{-# INLINE encodedBytes #-}

-- | Hash a value to a 'Multihash'.
--
-- Note that lazy 'ByteString's are /not/ an instance of 'ByteArrayAccess', ie.
-- you will have to pass a strict 'Strict.ByteString' here.
multihash :: (ByteArrayAccess ba, Multihashable a) => a -> ba -> Multihash
multihash rithm = fromDigest . C.hashWith rithm

-- | Decode a 'Multihash' from a lazy 'ByteString'.
decode :: ByteString -> Either String Multihash
decode = bimap _3 _3 . Binary.runGetOrFail getMultihash

-- | Decode a 'C.Digest' from a multihash-encoded lazy 'ByteString'.
decodeDigest
    :: forall a. Multihashable a
    => ByteString
    -> Either String (C.Digest a)
decodeDigest = bimap _3 _3 . Binary.runGetOrFail getMultihashedDigest

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
