{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

-- |
-- Copyright   : 2019 Monadic GmbH
-- License     : BSD3
-- Maintainer  : kim@monadic.xyz, alfredo@monadic.xyz, team@monadic.xyz
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- <https://github.com/multiformats/multibase multibase> encoding of strict 'ByteString's.
--

module Data.ByteString.Multibase
    ( -- * Supported bases
      BaseN.Base(..)
    , ToCode

    -- * Multibase encoding and decoding
    , Multibase
    , fromMultibase
    , encodedBytes
    , encode
    , decode

    -- * Compact represenation
    , CompactMultibase
    , compact
    , expand
    )
where

import           Control.DeepSeq (NFData)
import           Data.ByteString (ByteString)
import           Data.ByteString.BaseN (AtBase, ValidBase)
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString.Short (ShortByteString, fromShort, toShort)
import           Data.Coerce (coerce)
import           Data.Hashable (Hashable)
import           Data.Typeable

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Test.QuickCheck
-- >>> import qualified Data.ByteString as BS
-- >>> newtype Bytes = Bytes ByteString deriving (Eq, Show)
-- >>> instance Arbitrary Bytes where arbitrary = Bytes . BS.pack <$> arbitrary


-- Bases we don't support yet
--    Base1             -- ^ unary (11111) ----- WTF?
--  | Base2             -- ^ binary (01010101)
--  | Base8             -- ^ octal
--  | Base10            -- ^ decimal

-- | Symbols for which a multibase code is defined and supported by this library
class ValidBase b => ToCode b where
    toCode   :: proxy b -> Char

instance ToCode "id" where
    toCode = const '\000'
instance ToCode "16" where
    toCode = const 'f'
instance ToCode "16u" where
    toCode = const 'F'
instance ToCode "32x" where
    toCode = const 'v'
instance ToCode "32xu" where
    toCode = const 'V'
instance ToCode "32xp" where
    toCode = const 't'
instance ToCode "32xpu" where
    toCode = const 'T'
instance ToCode "32" where
    toCode = const 'b'
instance ToCode "32z" where
    toCode = const 'h'
instance ToCode "32u" where
    toCode = const 'B'
instance ToCode "32p" where
    toCode  = const 'c'
instance ToCode "32pu" where
    toCode  = const 'C'
instance ToCode "58flickr" where
    toCode  = const 'Z'
instance ToCode "58btc" where
    toCode  = const 'z'
instance ToCode "64" where
    toCode  = const 'm'
instance ToCode "64p" where
    toCode  = const 'M'
instance ToCode "64url" where
    toCode  = const 'u'
instance ToCode "64urlpad" where
    toCode  = const 'U'

-- codes we don't support yet
--toCode Base2             = '0'
--toCode Base8             = '7'
--toCode Base10            = '9'

fromCode :: Char -> Maybe (ByteString -> Either String ByteString)
fromCode '\000' = pure pure  -- 2 times pure, 2 times better!
fromCode 'f'    = pure (BaseN.decodeAtBaseEither (Proxy @"16"))
fromCode 'F'    = pure (BaseN.decodeAtBaseEither (Proxy @"16u"))
fromCode 'v'    = pure (BaseN.decodeAtBaseEither (Proxy @"32x"))
fromCode 'V'    = pure (BaseN.decodeAtBaseEither (Proxy @"32xu"))
fromCode 't'    = pure (BaseN.decodeAtBaseEither (Proxy @"32xp"))
fromCode 'T'    = pure (BaseN.decodeAtBaseEither (Proxy @"32xpu"))
fromCode 'b'    = pure (BaseN.decodeAtBaseEither (Proxy @"32"))
fromCode 'h'    = pure (BaseN.decodeAtBaseEither (Proxy @"32z"))
fromCode 'B'    = pure (BaseN.decodeAtBaseEither (Proxy @"32u"))
fromCode 'c'    = pure (BaseN.decodeAtBaseEither (Proxy @"32p"))
fromCode 'C'    = pure (BaseN.decodeAtBaseEither (Proxy @"32pu"))
fromCode 'Z'    = pure (BaseN.decodeAtBaseEither (Proxy @"58flickr"))
fromCode 'z'    = pure (BaseN.decodeAtBaseEither (Proxy @"58btc"))
fromCode 'm'    = pure (BaseN.decodeAtBaseEither (Proxy @"64"))
fromCode 'M'    = pure (BaseN.decodeAtBaseEither (Proxy @"64p"))
fromCode 'u'    = pure (BaseN.decodeAtBaseEither (Proxy @"64url"))
fromCode 'U'    = pure (BaseN.decodeAtBaseEither (Proxy @"64urlpad"))
fromCode _      = Nothing

-- | A multibase-encoded strict 'ByteString'.
newtype Multibase = Multibase ByteString
    deriving (Eq, Ord, Hashable, NFData)

-- | A 'Multibase backed by a 'ShortByteString'.
--
-- This is useful when holding many 'Multibase' values in memory, due to lower
-- memory overhead and less heap fragmentation. See the documentation for
-- 'ShortByteString' for details.
newtype CompactMultibase = Compact ShortByteString
    deriving (Eq, Ord, Hashable, NFData)

-- | Extract the encoded bytes, including the code 'Char', from a 'Multibase'.
fromMultibase :: Multibase -> ByteString
fromMultibase = coerce

-- | Extract the encoded bytes from a 'Multibase'.
--
-- The code 'Char' signifying the base is stripped.
encodedBytes :: Multibase -> ByteString
encodedBytes = C8.tail . coerce

-- | Encode a strict 'ByteString' at 'Base'.
encode :: ToCode b => AtBase b -> Multibase
encode base = coerce . C8.cons (toCode base) . BaseN.encodedBytes $ base

-- | Decode a strict 'ByteString', assumed to be 'Multibase'-encoded.
decode :: ByteString -> Either String ByteString
decode bs = do
    (c, bs') <- note "Empty input" $ C8.uncons bs
    case fromCode c of
      Just decodeIt -> decodeIt bs'
      Nothing       -> Left $ "Unknown encoding " <> show c

-- | /O(n)/. Convert a 'Multibase' encoding to a compact representation.
--
-- Involves copying the underlying 'ByteString'.
compact :: Multibase -> CompactMultibase
compact = coerce . toShort . coerce
{-# INLINE compact #-}

-- | /O(n)/. Convert from the compact to the regular representation.
--
-- Involves copying the underlying 'ShortByteString'.
expand :: CompactMultibase -> Multibase
expand = coerce . fromShort . coerce
{-# INLINE expand #-}

-- Helpers ---------------------------------------------------------------------

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) pure
