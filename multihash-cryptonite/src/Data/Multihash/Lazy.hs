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
-- Like "Data.Multihash", but a lazy 'ByteString' representation.
module Data.Multihash.Lazy
    ( Multihash
    , fromDigest
    , encodedBytes
    , multihash
    , decodeBytes

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
import qualified Data.ByteString as BS
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
    len   = buildVarInt $ BS.length bytes

-- | Extract the raw, multihash-encoded bytes of a 'Multihash'.
encodedBytes :: Multihash -> ByteString
encodedBytes = coerce
{-# INLINE encodedBytes #-}

-- | Hash a value to a 'Multihash'
multihash :: (ByteArrayAccess ba, Multihashable a) => a -> ba -> Multihash
multihash rithm = fromDigest . C.hashWith rithm

-- | Decode a 'C.Digest' from a multihash-encoded lazy 'ByteString'.
decodeBytes
    :: forall a. Multihashable a
    => ByteString
    -> Either String (C.Digest a)
decodeBytes = bimap _3 _3 . Binary.runGetOrFail getMultihash
  where
    _3 (_,_,x) = x
