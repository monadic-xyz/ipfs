{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Kitchen sink base-N encoding and decoding of strict 'ByteStrings'.
module Data.ByteString.Multibase
    ( -- * Supported bases
      Base (..)
    , toCode
    , fromCode

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

    -- * Low-level encoding and decoding
    , encodeBase16
    , decodeBase16
    , encodeBase16upper
    , decodeBase16upper
    , encodeBase32hex
    , decodeBase32hex
    , encodeBase32hexupper
    , decodeBase32hexupper
    , encodeBase32hexpad
    , decodeBase32hexpad
    , encodeBase32hexpadupper
    , decodeBase32hexpadupper
    , encodeBase32
    , decodeBase32
    , encodeBase32upper
    , decodeBase32upper
    , encodeBase32pad
    , decodeBase32pad
    , encodeBase32padupper
    , decodeBase32padupper
    , encodeBase58flickr
    , decodeBase58flickr
    , encodeBase58btc
    , decodeBase58btc
    , encodeBase64
    , decodeBase64
    , encodeBase64pad
    , decodeBase64pad
    , encodeBase64url
    , decodeBase64url
    , encodeBase64urlpad
    , decodeBase64urlpad
    )
where

import qualified Codec.Binary.Base32 as Base32
import qualified Codec.Binary.Base32Hex as Base32Hex
import           Control.DeepSeq (NFData)
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64Url
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString.Short (ShortByteString, fromShort, toShort)
import           Data.Char (toLower, toUpper)
import           Data.Coerce (coerce)
import           Data.Hashable (Hashable)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Test.QuickCheck
-- >>> import qualified Data.ByteString as BS
-- >>> newtype Bytes = Bytes ByteString deriving (Eq, Show)
-- >>> instance Arbitrary Bytes where arbitrary = Bytes . BS.pack <$> arbitrary


-- | Supported bases
data Base =
      Identity
--    Base1             -- ^ unary (11111) ----- WTF?
--  | Base2             -- ^ binary (01010101)
--  | Base8             -- ^ octal
--  | Base10            -- ^ decimal
    | Base16            -- ^ hexadecimal
    | Base16upper       -- ^ hexadecimal, uppercase alphabet
    | Base32hex         -- ^ RFC4648 no padding - highest char
    | Base32hexupper    -- ^ RFC4648 no padding - highest char, uppercase alphabet
    | Base32hexpad      -- ^ RFC4648 with padding
    | Base32hexpadupper -- ^ RFC4648 with padding, uppercase alphabet
    | Base32            -- ^ RFC4648 no padding
    | Base32upper       -- ^ RFC4648 no padding, uppercase alphabet
    | Base32pad         -- ^ RFC4648 with padding
    | Base32padupper    -- ^ RFC4648 with padding, uppercase alphabet
--  | Base32z           -- ^ z-base-32 (used by Tahoe-LAFS)
    | Base58flickr      -- ^ base58 flickr alphabet
    | Base58btc         -- ^ base58 bitcoint alphabet
    | Base64            -- ^ RFC4648 no padding
    | Base64pad         -- ^ RFC4648 with padding (MIME-encoding)
    | Base64url         -- ^ RFC4648 no padding
    | Base64urlpad      -- ^ RFC4648 with padding

toCode :: Base -> Char
toCode Identity          = '\000'
--toCode Base2             = '0'
--toCode Base8             = '7'
--toCode Base10            = '9'
toCode Base16            = 'f'
toCode Base16upper       = 'F'
toCode Base32hex         = 'v'
toCode Base32hexupper    = 'V'
toCode Base32hexpad      = 't'
toCode Base32hexpadupper = 'T'
toCode Base32            = 'b'
toCode Base32upper       = 'B'
toCode Base32pad         = 'c'
toCode Base32padupper    = 'C'
--toCode Base32z           = 'h'
toCode Base58flickr      = 'Z'
toCode Base58btc         = 'z'
toCode Base64            = 'm'
toCode Base64pad         = 'M'
toCode Base64url         = 'u'
toCode Base64urlpad      = 'U'

fromCode :: Char -> Maybe Base
fromCode '\000' = pure Identity
--fromCode '0'    = pure Base2
--fromCode '7'    = pure Base8
--fromCode '9'    = pure Base10
fromCode 'f'    = pure Base16
fromCode 'F'    = pure Base16upper
fromCode 'v'    = pure Base32hex
fromCode 'V'    = pure Base32hexupper
fromCode 't'    = pure Base32hexpad
fromCode 'T'    = pure Base32hexpadupper
fromCode 'b'    = pure Base32
fromCode 'B'    = pure Base32upper
fromCode 'c'    = pure Base32pad
fromCode 'C'    = pure Base32padupper
--fromCode 'h'    = pure Base32z
fromCode 'Z'    = pure Base58flickr
fromCode 'z'    = pure Base58btc
fromCode 'm'    = pure Base64
fromCode 'M'    = pure Base64pad
fromCode 'u'    = pure Base64url
fromCode 'U'    = pure Base64urlpad
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
encode :: Base -> ByteString -> Multibase
encode base bs = coerce $ C8.cons (toCode base) $ encoder base bs

-- | Decode a strict 'ByteString', assumed to be 'Multibase'-encoded.
decode :: ByteString -> Either String ByteString
decode bs = do
    (c, bs') <- note "Empty input"      $ C8.uncons bs
    base     <- note ("Unknown encoding " <> show c) $ fromCode c
    decoder base bs'

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

-- Low-level encoding and decoding ---------------------------------------------

-- Base 16 ---------------------------------------------------------------------

-- |
-- >>> encodeBase16 "hello world"
-- "68656c6c6f20776f726c64"
encodeBase16 :: ByteString -> ByteString
encodeBase16 = Base16.encode
{-# INLINE encodeBase16 #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase16 (encodeBase16 bytes) === Right bytes
decodeBase16 :: ByteString -> Either String ByteString
decodeBase16 bs =
    case Base16.decode bs of
        (x, "")      -> Right x
        (x, invalid) -> Left . mconcat $
            [ "Decoded "
            , "`", unpack bs, "`"
            , " to "
            , "`", unpack x, "`"
            , " until invalid sequence: "
            , "`", unpack invalid, "`"
            ]
{-# INLINE decodeBase16 #-}

-- |
-- >>> encodeBase16upper "hello world"
-- "68656C6C6F20776F726C64"
encodeBase16upper :: ByteString -> ByteString
encodeBase16upper = C8.map toUpper . Base16.encode
{-# INLINE encodeBase16upper #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase16upper (encodeBase16upper bytes) === Right bytes
decodeBase16upper :: ByteString -> Either String ByteString
decodeBase16upper = decodeBase16
{-# INLINE decodeBase16upper #-}

-- Base 32 ---------------------------------------------------------------------

-- |
-- encodeBase32hex "hello world"
-- "d1imor3f41rmusjccg"
encodeBase32hex :: ByteString -> ByteString
encodeBase32hex = C8.map toLower . dropPadding . Base32Hex.encode
{-# INLINE encodeBase32hex #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32hex (encodeBase32hex bytes) === Right bytes
decodeBase32hex :: ByteString -> Either String ByteString
decodeBase32hex bs
    | C8.null bs = pure mempty
    | otherwise  = first (base32Err bs) . Base32Hex.decode . padTo 8 . C8.map toUpper $ bs
{-# INLINE decodeBase32hex #-}

-- |
-- encodeBase32hexupper "hello world"
-- "D1IMOR3F41RMUSJCCG"
encodeBase32hexupper :: ByteString -> ByteString
encodeBase32hexupper = dropPadding . Base32Hex.encode
{-# INLINE encodeBase32hexupper #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32hexupper (encodeBase32hexupper bytes) === Right bytes
decodeBase32hexupper :: ByteString -> Either String ByteString
decodeBase32hexupper bs = first (base32Err bs) . Base32Hex.decode . padTo 8 $ bs
{-# INLINE decodeBase32hexupper #-}

-- |
-- >>> encodeBase32hexpad "hello world"
-- "d1imor3f41rmusjccg======"
encodeBase32hexpad :: ByteString -> ByteString
encodeBase32hexpad = C8.map toLower . Base32Hex.encode
{-# INLINE encodeBase32hexpad #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32hexpad (encodeBase32hexpad bytes) === Right bytes
decodeBase32hexpad :: ByteString -> Either String ByteString
decodeBase32hexpad bs =
    first (base32Err bs) . Base32Hex.decode . C8.map toUpper $ bs
{-# INLINE decodeBase32hexpad #-}

-- |
-- >>> encodeBase32hexpadupper "hello world"
-- "D1IMOR3F41RMUSJCCG======"
encodeBase32hexpadupper :: ByteString -> ByteString
encodeBase32hexpadupper = Base32Hex.encode
{-# INLINE encodeBase32hexpadupper #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32hexpadupper (encodeBase32hexpadupper bytes) === Right bytes
decodeBase32hexpadupper :: ByteString -> Either String ByteString
decodeBase32hexpadupper bs = first (base32Err bs) . Base32Hex.decode $ bs
{-# INLINE decodeBase32hexpadupper #-}

-- |
-- >>> encodeBase32 "hello world"
-- "nbswy3dpeb3w64tmmq"
encodeBase32 :: ByteString -> ByteString
encodeBase32 = C8.map toLower . dropPadding . Base32.encode
{-# INLINE encodeBase32 #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32 (encodeBase32 bytes) === Right bytes
decodeBase32 :: ByteString -> Either String ByteString
decodeBase32 bs =
    first (base32Err bs) . Base32.decode . padTo 8 . C8.map toUpper $ bs
{-# INLINE decodeBase32 #-}

-- |
-- >>> encodeBase32upper "hello world"
-- "NBSWY3DPEB3W64TMMQ"
encodeBase32upper :: ByteString -> ByteString
encodeBase32upper = dropPadding . Base32.encode
{-# INLINE encodeBase32upper #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32upper (encodeBase32upper bytes) === Right bytes
decodeBase32upper :: ByteString -> Either String ByteString
decodeBase32upper bs = first (base32Err bs) . Base32.decode . padTo 8 $ bs
{-# INLINE decodeBase32upper #-}

-- |
-- >>> encodeBase32pad "hello world"
-- "nbswy3dpeb3w64tmmq======"
encodeBase32pad :: ByteString -> ByteString
encodeBase32pad = C8.map toLower . Base32.encode
{-# INLINE encodeBase32pad #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32pad (encodeBase32pad bytes) === Right bytes
decodeBase32pad :: ByteString -> Either String ByteString
decodeBase32pad bs = first (base32Err bs) . Base32.decode . C8.map toUpper $ bs
{-# INLINE decodeBase32pad #-}

-- |
-- >>> encodeBase32padupper "hello world"
-- "NBSWY3DPEB3W64TMMQ======"
encodeBase32padupper :: ByteString -> ByteString
encodeBase32padupper = Base32.encode
{-# INLINE encodeBase32padupper #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32padupper (encodeBase32padupper bytes) === Right bytes
decodeBase32padupper :: ByteString -> Either String ByteString
decodeBase32padupper bs = first (base32Err bs) . Base32.decode $ bs
{-# INLINE decodeBase32padupper #-}

base32Err :: ByteString -> (ByteString, ByteString) -> String
base32Err orig (x, invalid) = mconcat
    [ "Decoded "
    , "`", unpack orig, "`"
    , " to "
    , "`", unpack x, "`"
    , " until invalid sequence: "
    , "`", unpack invalid, "`"
    ]

-- Base 58 ---------------------------------------------------------------------

-- |
-- >>> encodeBase58flickr "hello world"
-- "rTu1dk6cWsRYjYu"
encodeBase58flickr :: ByteString -> ByteString
encodeBase58flickr = Base58.encodeBase58 Base58.flickrAlphabet
{-# INLINE encodeBase58flickr #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase58flickr (encodeBase58flickr bytes) === Right bytes
decodeBase58flickr :: ByteString -> Either String ByteString
decodeBase58flickr =
    note "Invalid characters in Base58flickr string"
        . Base58.decodeBase58 Base58.flickrAlphabet
{-# INLINE decodeBase58flickr #-}

-- |
-- >>> encodeBase58btc "hello world"
-- "StV1DL6CwTryKyV"
encodeBase58btc :: ByteString -> ByteString
encodeBase58btc = Base58.encodeBase58 Base58.bitcoinAlphabet
{-# INLINE encodeBase58btc #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase58btc (encodeBase58btc bytes) === Right bytes
decodeBase58btc :: ByteString -> Either String ByteString
decodeBase58btc =
    note "Invalid characters in Base58btc string"
        . Base58.decodeBase58 Base58.bitcoinAlphabet
{-# INLINE decodeBase58btc #-}

-- Base 64 ---------------------------------------------------------------------

-- |
-- >>> encodeBase64 "hello world"
-- "aGVsbG8gd29ybGQ"
encodeBase64 :: ByteString -> ByteString
encodeBase64 = fst . C8.spanEnd (== '=') . Base64.encode
{-# INLINE encodeBase64 #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase64 (encodeBase64 bytes) === Right bytes
decodeBase64 :: ByteString -> Either String ByteString
decodeBase64 = Base64.decode . padTo 4
{-# INLINE decodeBase64 #-}

-- |
-- >>> encodeBase64pad "hello world"
-- "aGVsbG8gd29ybGQ="
encodeBase64pad :: ByteString -> ByteString
encodeBase64pad = Base64.encode
{-# INLINE encodeBase64pad #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase64pad (encodeBase64pad bytes) === Right bytes
decodeBase64pad :: ByteString -> Either String ByteString
decodeBase64pad = Base64.decode
{-# INLINE decodeBase64pad #-}

-- |
-- >>> encodeBase64url "hello world"
-- "aGVsbG8gd29ybGQ"
encodeBase64url :: ByteString -> ByteString
encodeBase64url = fst . C8.spanEnd (== '=') . Base64Url.encode
{-# INLINE encodeBase64url #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase64url (encodeBase64url bytes) === Right bytes
decodeBase64url :: ByteString -> Either String ByteString
decodeBase64url = Base64Url.decode . padTo 4
{-# INLINE decodeBase64url #-}

-- |
-- >>> encodeBase64urlpad "hello world"
-- "aGVsbG8gd29ybGQ="
encodeBase64urlpad :: ByteString -> ByteString
encodeBase64urlpad = Base64Url.encode
{-# INLINE encodeBase64urlpad #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase64urlpad (encodeBase64urlpad bytes) === Right bytes
decodeBase64urlpad :: ByteString -> Either String ByteString
decodeBase64urlpad = Base64Url.decode
{-# INLINE decodeBase64urlpad #-}

-- Internal --------------------------------------------------------------------

encoder :: Base -> ByteString -> ByteString
encoder Identity          = id
--encoder Base2             = undefined
--encoder Base8             = undefined
--encoder Base10            = undefined
encoder Base16            = encodeBase16
encoder Base16upper       = encodeBase16upper
encoder Base32hex         = encodeBase32hex
encoder Base32hexupper    = encodeBase32hexupper
encoder Base32hexpad      = encodeBase32hexpad
encoder Base32hexpadupper = encodeBase32hexpadupper
encoder Base32            = encodeBase32
encoder Base32upper       = encodeBase32upper
encoder Base32pad         = encodeBase32pad
encoder Base32padupper    = encodeBase32padupper
--encoder Base32z           . undefined
encoder Base58flickr      = encodeBase58flickr
encoder Base58btc         = encodeBase58btc
encoder Base64            = encodeBase64
encoder Base64pad         = encodeBase64pad
encoder Base64url         = encodeBase64url
encoder Base64urlpad      = encodeBase64urlpad

decoder :: Base -> ByteString -> Either String ByteString
decoder Identity          = pure
-- decoder Base2             = undefined
-- decoder Base8             = undefined
-- decoder Base10            = undefined
decoder Base16            = decodeBase16
decoder Base16upper       = decodeBase16upper
decoder Base32hex         = decodeBase32hex
decoder Base32hexupper    = decodeBase32hexupper
decoder Base32hexpad      = decodeBase32hexpad
decoder Base32hexpadupper = decodeBase32hexpadupper
decoder Base32            = decodeBase32
decoder Base32upper       = decodeBase32upper
decoder Base32pad         = decodeBase32pad
decoder Base32padupper    = decodeBase32padupper
-- decoder Base32z           = undefined
decoder Base58flickr      = decodeBase58flickr
decoder Base58btc         = decodeBase58btc
decoder Base64            = decodeBase64
decoder Base64pad         = decodeBase64pad
decoder Base64url         = decodeBase64url
decoder Base64urlpad      = decodeBase64urlpad

-- Helpers ---------------------------------------------------------------------

dropPadding :: ByteString -> ByteString
dropPadding = fst . C8.spanEnd (== '=')
{-# INLINE dropPadding #-}

padTo :: Int -> ByteString -> ByteString
padTo multipleof bs =
    case C8.length bs `mod` multipleof of
        0 -> bs
        x -> mappend bs (C8.replicate (multipleof - x) '=')

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) pure
