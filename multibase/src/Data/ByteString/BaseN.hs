{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

-- |
-- Copyright   : 2019 Monadic GmbH
-- License     : BSD3
-- Maintainer  : kim@monadic.xyz, alfredo@monadic.xyz, team@monadic.xyz
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Type-safe kitchen sink base-N encoding and decoding of strict 'ByteString's.
--

module Data.ByteString.BaseN
    ( Base(..)

    , AtBase
    , encodedBytes
    , encodedBuilder

    , Base2
    , Base16
    , Base58
    , Base64
    , BaseIdentity
    , Base16upper
    , Base32hex
    , Base32hexupper
    , Base32hexpad
    , Base32hexpadupper
    , Base32
    , Base32z
    , Base32upper
    , Base32pad
    , Base32padupper
    , Base58flickr
    , Base58btc
    , Base64pad
    , Base64url
    , Base64urlpad
    -- * Compact Representation
    , AtBaseCompact
    , compact
    , expand

    -- * Tagged
    -- $tagged
    , Base16Of
    , Base58Of
    , Base64Of

    -- ** Re-exports
    , tagWith
    , unTagged

    -- ** CBOR
    -- $cbor
    , DeserialiseError(..)
    , deserialiseAtBase

    -- * Encoding
    -- $encoding
    , encodeBase16
    , encodeBase58btc
    , encodeBase64
    , encodeAtBase

    -- * Decoding Bytes
    -- $decodingbytes
    , DecodeBase
    , decodeBase16
    , decodeBase16Either
    , decodeBase58btc
    , decodeBase64
    , decodeBase64Either
    , decodeBase64Lenient
    , decodeAtBase
    , decodeAtBaseEither

    -- * Decoding
    , decode

    -- ** Untrusted Input
    -- $untrusted
    , ValidBase
    , validBase16
    , validBase16Either
    , validBase58btc
    , validBase58btcEither
    , validBase64
    , validBase64Either
    , validAtBase
    , validAtBaseEither

    , validAndDecoded
    , validAndDecodedEither

    -- * 'Text'
    , encodedTextAtBase
    , encodedText
    , encodedTextBuilder

    -- * 'Formatting'
    , format
    , formatAtBase
    ) where

import           Prelude

import qualified Codec.Binary.Base32 as Base32
import qualified Codec.Binary.Base32Hex as Base32Hex
import           Codec.Serialise
                 ( DeserialiseFailure
                 , Serialise
                 , deserialiseOrFail
                 )
import           Control.DeepSeq (NFData)
import           Data.Aeson
                 ( FromJSON(..)
                 , FromJSONKey
                 , ToJSON(..)
                 , ToJSONKey
                 , withText
                 )
import qualified Data.Aeson.Encoding as JSON
import           Data.Bifunctor (bimap, first, second)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base32.Z as Base32z
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64Url
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString.Lazy (fromStrict)
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.Char (toLower, toUpper)
import           Data.Hashable (Hashable)
import           Data.Proxy (Proxy(..))
import           Data.String (IsString(..))
import           Data.Tagged (Tagged, tagWith, unTagged)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import qualified Data.Text.Lazy.Builder as T
import qualified Formatting
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import           Text.Show (Show(..), showParen, showString)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Test.QuickCheck
-- >>> import qualified Data.ByteString as BS
-- >>> newtype Bytes = Bytes ByteString deriving (Eq, Show)
-- >>> instance Arbitrary Bytes where arbitrary = Bytes . BS.pack <$> arbitrary

-- | Supported bases.
data Base (a :: Symbol) where
    BaseIdentity      :: Base "id"
    Base2             :: Base  "2"
    Base16            :: Base "16"
    Base64            :: Base "64"
    -- | hexadecimal, uppercase alphabet
    Base16upper       :: Base "16u"
    -- | RFC4648 no padding - highest char
    Base32hex         :: Base "32x"
    -- | RFC4648 no padding - highest char, uppercase alphabet
    Base32hexupper    :: Base "32xu"
    -- | RFC4648 with padding
    Base32hexpad      :: Base "32xp"
    -- | RFC4648 with padding, uppercase alphabet
    Base32hexpadupper :: Base "32xpu"
    -- | RFC4648 no padding
    Base32            :: Base "32"
    -- | z-base-32 (used by Tahoe-LAFS)
    Base32z           :: Base "32z"
    -- | RFC4648 no padding, uppercase alphabet
    Base32upper       :: Base "32u"
    -- | RFC4648 with padding
    Base32pad         :: Base "32p"
    -- | RFC4648 with padding, uppercase alphabet
    Base32padupper    :: Base "32pu"
    -- | base58 flickr alphabet
    Base58flickr      :: Base "58flickr"
    -- | base58 bitcoint alphabet
    Base58btc         :: Base "58btc"
    -- | RFC4648 with padding (MIME-encoding)
    Base64pad         :: Base "64p"
    -- | RFC4648 no padding
    Base64url         :: Base "64url"
    -- | RFC4648 with padding
    Base64urlpad      :: Base "64urlp"

-- | A 'ByteString' encoded at a specific base.
newtype AtBase (b :: Symbol) = BaseN { fromAtBase :: ByteString }
    deriving (Eq, Ord, NFData, Hashable)

-- | Extract the base-n encoded bytes from an 'AtBase'.
--
-- To recover the original 'ByteString' (*not* base-n encoded), use 'decode'.
encodedBytes :: AtBase b -> ByteString
encodedBytes (BaseN bs) = bs

-- | Like 'encodedBytes', but return a 'Builder'.
encodedBuilder :: AtBase b -> Builder
encodedBuilder = Builder.byteString . encodedBytes

instance KnownSymbol b => Show (AtBase b) where
    showsPrec p (BaseN bs) = showParen (p >= 11)
        ( showString ("Base" <> show (symbolVal (Proxy @b)) <> " ")
        . showsPrec 11 bs
        )

instance ValidBase b => IsString (AtBase b) where
    fromString = either error id . validAtBaseEither (Proxy @b) . C8.pack

instance ToJSON (AtBase b) where
    toJSON     = toJSON . encodedText
    toEncoding = JSON.text . encodedText

instance (ValidBase b, KnownSymbol b) => FromJSON (AtBase b) where
    parseJSON =
        withText ("AtBase " <> show (symbolVal (Proxy @b))) $
            either fail pure . validAtBaseEither (Proxy @b) . encodeUtf8

instance ToJSONKey (AtBase b)
instance (ValidBase b, KnownSymbol b) => FromJSONKey (AtBase b)

type Base2             = AtBase  "2"
type Base16            = AtBase "16"
type Base58            = AtBase "58"
type Base64            = AtBase "64"
type BaseIdentity      = AtBase "id"
type Base16upper       = AtBase "16u"
type Base32hex         = AtBase "16x"
type Base32hexupper    = AtBase "16xu"
type Base32hexpad      = AtBase "16xp"
type Base32hexpadupper = AtBase "16xup"
type Base32            = AtBase "32"
type Base32z           = AtBase "32z"
type Base32upper       = AtBase "32u"
type Base32pad         = AtBase "32p"
type Base32padupper    = AtBase "32pu"
type Base58flickr      = AtBase "58flickr"
type Base58btc         = AtBase "58btc"
type Base64pad         = AtBase "64p"
type Base64url         = AtBase "64url"
type Base64urlpad      = AtBase "16urlp"

-- Compact ---------------------------------------------------------------------

-- | A more memory-efficient representation of base-n encoded bytes.
--
-- Uses 'ShortByteString', recommendations and caveats described there apply.
newtype AtBaseCompact (b :: Symbol) = BaseNShort
    { fromAtBaseCompact :: ShortByteString
    } deriving (Eq, Ord, Hashable, NFData)

instance KnownSymbol b => Show (AtBaseCompact b) where
    showsPrec p (BaseNShort bs) = showParen (p >= 11)
        ( showString ("Base" <> show (symbolVal (Proxy @b)) <> "Compact ")
        . showsPrec 11 bs
        )

compact :: AtBase b -> AtBaseCompact b
compact = BaseNShort . Short.toShort . fromAtBase

expand :: AtBaseCompact b -> AtBase b
expand = BaseN . Short.fromShort . fromAtBaseCompact

-- $tagged
-- 'AtBase' values tagged by the type they're representing.

type Base16Of a = Tagged a (AtBase "16")
type Base58Of a = Tagged a (AtBase "58")
type Base64Of a = Tagged a (AtBase "64")

-- $cbor
-- Directly go from (presumed to be) base-n encoded 'ByteString' to
-- de-'Serialise'-able value.

data DeserialiseError =
      DecodeBaseError  String
    | DeserialiseError DeserialiseFailure
    deriving Show

deserialiseAtBase
    :: ( Serialise  a
       , DecodeBase b
       )
    => proxy b
    -> ByteString
    -> Either DeserialiseError a
deserialiseAtBase base bs = do
    bs' <- bimap DecodeBaseError fromStrict $ decodeAtBaseEither base bs
    first DeserialiseError $ deserialiseOrFail bs'

-- $encoding

encodeBase16 :: ByteString -> AtBase "16"
encodeBase16 = BaseN . Base16.encode
{-# INLINE encodeBase16 #-}

encodeBase64 :: ByteString -> AtBase "64"
encodeBase64 = BaseN . Base64.encode
{-# INLINE encodeBase64 #-}

-- |
-- >>> fromAtBase $ encodeBase16upper "hello world"
-- "68656C6C6F20776F726C64"
encodeBase16upper :: ByteString -> AtBase "16u"
encodeBase16upper = BaseN . C8.map toUpper . Base16.encode
{-# INLINE encodeBase16upper #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase16upper (fromAtBase $ encodeBase16upper bytes) === Right bytes
decodeBase16upper :: ByteString -> Either String ByteString
decodeBase16upper = decodeBase16Either
{-# INLINE decodeBase16upper #-}

-- Base 32 ---------------------------------------------------------------------

-- |
-- >>> fromAtBase . encodeBase32hex $ "hello world"
-- "d1imor3f41rmusjccg"
encodeBase32hex :: ByteString -> AtBase "32x"
encodeBase32hex = BaseN . C8.map toLower . dropPadding . Base32Hex.encode
{-# INLINE encodeBase32hex #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32hex (fromAtBase $ encodeBase32hex bytes) === Right bytes
decodeBase32hex :: ByteString -> Either String ByteString
decodeBase32hex bs
    | C8.null bs = pure mempty
    | otherwise  = first (base32Err bs) . Base32Hex.decode . padTo 8 . C8.map toUpper $ bs
{-# INLINE decodeBase32hex #-}

-- |
-- >>> fromAtBase . encodeBase32hexupper $ "hello world"
-- "D1IMOR3F41RMUSJCCG"
encodeBase32hexupper :: ByteString -> AtBase "32xu"
encodeBase32hexupper = BaseN . dropPadding . Base32Hex.encode
{-# INLINE encodeBase32hexupper #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32hexupper (fromAtBase $ encodeBase32hexupper bytes) === Right bytes
decodeBase32hexupper :: ByteString -> Either String ByteString
decodeBase32hexupper bs = first (base32Err bs) . Base32Hex.decode . padTo 8 $ bs
{-# INLINE decodeBase32hexupper #-}

-- |
-- >>> fromAtBase . encodeBase32hexpad $ "hello world"
-- "d1imor3f41rmusjccg======"
encodeBase32hexpad :: ByteString -> AtBase "32xp"
encodeBase32hexpad = BaseN . C8.map toLower . Base32Hex.encode
{-# INLINE encodeBase32hexpad #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32hexpad (fromAtBase $ encodeBase32hexpad bytes) === Right bytes
decodeBase32hexpad :: ByteString -> Either String ByteString
decodeBase32hexpad bs =
    first (base32Err bs) . Base32Hex.decode . C8.map toUpper $ bs
{-# INLINE decodeBase32hexpad #-}

-- |
-- >>> fromAtBase . encodeBase32hexpadupper $ "hello world"
-- "D1IMOR3F41RMUSJCCG======"
encodeBase32hexpadupper :: ByteString -> AtBase "32xpu"
encodeBase32hexpadupper = BaseN . Base32Hex.encode
{-# INLINE encodeBase32hexpadupper #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32hexpadupper (fromAtBase $ encodeBase32hexpadupper bytes) === Right bytes
decodeBase32hexpadupper :: ByteString -> Either String ByteString
decodeBase32hexpadupper bs = first (base32Err bs) . Base32Hex.decode $ bs
{-# INLINE decodeBase32hexpadupper #-}

-- |
-- >>> fromAtBase . encodeBase32 $ "hello world"
-- "nbswy3dpeb3w64tmmq"
encodeBase32 :: ByteString -> AtBase "32"
encodeBase32 = BaseN . C8.map toLower . dropPadding . Base32.encode
{-# INLINE encodeBase32 #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32 (fromAtBase $ encodeBase32 bytes) === Right bytes
decodeBase32 :: ByteString -> Either String ByteString
decodeBase32 bs =
    first (base32Err bs) . Base32.decode . padTo 8 . C8.map toUpper $ bs
{-# INLINE decodeBase32 #-}

-- |
-- >>> fromAtBase . encodeBase32z $ "hello world"
-- "pb1sa5dxrb5s6hucco"
encodeBase32z :: ByteString -> AtBase "32z"
encodeBase32z = BaseN  . Base32z.encode
{-# INLINE encodeBase32z #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32z (fromAtBase $ encodeBase32z bytes) === Right bytes
decodeBase32z :: ByteString -> Either String ByteString
decodeBase32z = Base32z.decode . C8.map toLower
{-# INLINE decodeBase32z #-}
-- |
-- >>> fromAtBase . encodeBase32upper $ "hello world"
-- "NBSWY3DPEB3W64TMMQ"
encodeBase32upper :: ByteString -> AtBase "32u"
encodeBase32upper = BaseN . dropPadding . Base32.encode
{-# INLINE encodeBase32upper #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32upper (fromAtBase $ encodeBase32upper bytes) === Right bytes
decodeBase32upper :: ByteString -> Either String ByteString
decodeBase32upper bs = first (base32Err bs) . Base32.decode . padTo 8 $ bs
{-# INLINE decodeBase32upper #-}

-- |
-- >>> fromAtBase . encodeBase32pad $ "hello world"
-- "nbswy3dpeb3w64tmmq======"
encodeBase32pad :: ByteString -> AtBase "32p"
encodeBase32pad = BaseN . C8.map toLower . Base32.encode
{-# INLINE encodeBase32pad #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32pad (fromAtBase $ encodeBase32pad bytes) === Right bytes
decodeBase32pad :: ByteString -> Either String ByteString
decodeBase32pad bs = first (base32Err bs) . Base32.decode . C8.map toUpper $ bs
{-# INLINE decodeBase32pad #-}

-- |
-- >>> fromAtBase . encodeBase32padupper $ "hello world"
-- "NBSWY3DPEB3W64TMMQ======"
encodeBase32padupper :: ByteString -> AtBase "32pu"
encodeBase32padupper = BaseN . Base32.encode
{-# INLINE encodeBase32padupper #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase32padupper (fromAtBase $ encodeBase32padupper bytes) === Right bytes
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
-- >>> fromAtBase . encodeBase58flickr $ "hello world"
-- "rTu1dk6cWsRYjYu"
encodeBase58flickr :: ByteString -> AtBase "58flickr"
encodeBase58flickr = BaseN . Base58.encodeBase58 Base58.flickrAlphabet
{-# INLINE encodeBase58flickr #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase58flickr (fromAtBase $ encodeBase58flickr bytes) === Right bytes
decodeBase58flickr :: ByteString -> Either String ByteString
decodeBase58flickr =
    note "Invalid characters in Base58flickr string"
        . Base58.decodeBase58 Base58.flickrAlphabet
{-# INLINE decodeBase58flickr #-}

-- |
-- >>> fromAtBase . encodeBase58btc $ "hello world"
-- "StV1DL6CwTryKyV"
encodeBase58btc :: ByteString -> AtBase "58btc"
encodeBase58btc = BaseN . Base58.encodeBase58 Base58.bitcoinAlphabet
{-# INLINE encodeBase58btc #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase58btc (fromAtBase $ encodeBase58btc bytes) === Right bytes
decodeBase58btc :: ByteString -> Either String ByteString
decodeBase58btc =
    note "Invalid characters in Base58btc string"
        . Base58.decodeBase58 Base58.bitcoinAlphabet
{-# INLINE decodeBase58btc #-}

-- |
-- >>> fromAtBase . encodeBase64pad $ "hello world"
-- "aGVsbG8gd29ybGQ="
encodeBase64pad :: ByteString -> AtBase "64p"
encodeBase64pad = BaseN . Base64.encode
{-# INLINE encodeBase64pad #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase64pad (fromAtBase $ encodeBase64pad bytes) === Right bytes
decodeBase64pad :: ByteString -> Either String ByteString
decodeBase64pad = Base64.decode
{-# INLINE decodeBase64pad #-}

-- |
-- >>> fromAtBase . encodeBase64url $ "hello world"
-- "aGVsbG8gd29ybGQ"
encodeBase64url :: ByteString -> AtBase "64url"
encodeBase64url = BaseN . fst . C8.spanEnd (== '=') . Base64Url.encode
{-# INLINE encodeBase64url #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase64url (fromAtBase $ encodeBase64url bytes) === Right bytes
decodeBase64url :: ByteString -> Either String ByteString
decodeBase64url = Base64Url.decode . padTo 4
{-# INLINE decodeBase64url #-}

-- |
-- >>> fromAtBase . encodeBase64urlpad $ "hello world"
-- "aGVsbG8gd29ybGQ="
encodeBase64urlpad :: ByteString -> AtBase "64urlp"
encodeBase64urlpad = BaseN . Base64Url.encode
{-# INLINE encodeBase64urlpad #-}

-- |
-- prop> \(Bytes bytes) -> decodeBase64urlpad (fromAtBase $ encodeBase64urlpad bytes) === Right bytes
decodeBase64urlpad :: ByteString -> Either String ByteString
decodeBase64urlpad = Base64Url.decode
{-# INLINE decodeBase64urlpad #-}

-- | Encode at a base supplied at runtime.
encodeAtBase :: Base b -> ByteString -> AtBase b
encodeAtBase Base2             = BaseN
encodeAtBase Base16            = encodeBase16
encodeAtBase Base64            = encodeBase64
encodeAtBase BaseIdentity      = BaseN
encodeAtBase Base16upper       = encodeBase16upper
encodeAtBase Base32hex         = encodeBase32hex
encodeAtBase Base32hexupper    = encodeBase32hexupper
encodeAtBase Base32hexpad      = encodeBase32hexpad
encodeAtBase Base32hexpadupper = encodeBase32hexpadupper
encodeAtBase Base32            = encodeBase32
encodeAtBase Base32z           = encodeBase32z
encodeAtBase Base32upper       = encodeBase32upper
encodeAtBase Base32pad         = encodeBase32pad
encodeAtBase Base32padupper    = encodeBase32padupper
encodeAtBase Base58flickr      = encodeBase58flickr
encodeAtBase Base58btc         = encodeBase58btc
encodeAtBase Base64pad         = encodeBase64pad
encodeAtBase Base64url         = encodeBase64url
encodeAtBase Base64urlpad      = encodeBase64urlpad

-- $decodingbytes
-- Decode (presumed to be) base-n encoded 'ByteString's to their original
-- (base-2) value.

decodeBase16 :: ByteString -> Maybe ByteString
decodeBase16 = either (const Nothing) pure . decodeBase16Either

decodeBase16Either :: ByteString -> Either String ByteString
decodeBase16Either bs =
    case Base16.decode bs of
        (x, "")      -> Right x
        (x, invalid) -> Left . mconcat $
            [ "Decoded: "
            , "`", unpack x, "`"
            , " until invalid sequence: "
            , "`", unpack invalid, "`"
            ]
{-# INLINE decodeBase16Either #-}

decodeBase64 :: ByteString -> Maybe ByteString
decodeBase64 = either (const Nothing) pure . decodeBase64Either

decodeBase64Either :: ByteString -> Either String ByteString
decodeBase64Either = Base64.decode
{-# INLINE decodeBase64Either #-}

decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = Base64.decodeLenient
{-# INLINE decodeBase64Lenient #-}

class DecodeBase (b :: Symbol) where
    decodeAtBase       :: proxy b -> ByteString -> Maybe ByteString
    decodeAtBaseEither :: proxy b -> ByteString -> Either String ByteString

instance DecodeBase "id" where
    decodeAtBase       = const pure
    decodeAtBaseEither = const pure
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "2" where
    decodeAtBase       = const pure
    decodeAtBaseEither = const pure
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "16" where
    decodeAtBase       = const decodeBase16
    decodeAtBaseEither = const decodeBase16Either
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "16u" where
    decodeAtBase       = const (hush . decodeBase16upper)
    decodeAtBaseEither = const decodeBase16upper
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "32" where
    decodeAtBase       = const (hush . decodeBase32)
    decodeAtBaseEither = const decodeBase32
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "32z" where
    decodeAtBase       = const (hush . decodeBase32z)
    decodeAtBaseEither = const decodeBase32z
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "32u" where
    decodeAtBase       = const (hush . decodeBase32upper)
    decodeAtBaseEither = const decodeBase32upper
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "32p" where
    decodeAtBase       = const (hush . decodeBase32pad)
    decodeAtBaseEither = const decodeBase32pad
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "32pu" where
    decodeAtBase       = const (hush . decodeBase32padupper)
    decodeAtBaseEither = const decodeBase32padupper
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "32x" where
    decodeAtBase       = const (hush . decodeBase32hex)
    decodeAtBaseEither = const decodeBase32hex
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "32xu" where
    decodeAtBase       = const (hush . decodeBase32hexupper)
    decodeAtBaseEither = const decodeBase32hexupper
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "32xp" where
    decodeAtBase       = const (hush . decodeBase32hexpad)
    decodeAtBaseEither = const decodeBase32hexpad
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "32xpu" where
    decodeAtBase       = const (hush . decodeBase32hexpadupper)
    decodeAtBaseEither = const decodeBase32hexpadupper
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "58btc" where
    decodeAtBase       = const (hush . decodeBase58btc)
    decodeAtBaseEither = const decodeBase58btc
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "58flickr" where
    decodeAtBase       = const (hush . decodeBase58flickr)
    decodeAtBaseEither = const decodeBase58flickr
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "64" where
    decodeAtBase       = const decodeBase64
    decodeAtBaseEither = const decodeBase64Either
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "64p" where
    decodeAtBase       = const (hush . decodeBase64pad)
    decodeAtBaseEither = const decodeBase64pad
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "64url" where
    decodeAtBase       = const (hush . decodeBase64url)
    decodeAtBaseEither = const decodeBase64url
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

instance DecodeBase "64urlpad" where
    decodeAtBase       = const (hush . decodeBase64urlpad)
    decodeAtBaseEither = const decodeBase64urlpad
    {-# INLINE decodeAtBase       #-}
    {-# INLINE decodeAtBaseEither #-}

-- | Recover the original 'ByteString' of a base-n encoded value.
decode :: DecodeBase b => AtBase b -> ByteString
decode at = case decodeAtBaseEither at (encodedBytes at) of
    Left  e -> error $ "Impossible: invalid base encoding: " <> e
    Right b -> b

-- $untrusted
-- Construct 'AtBase's from raw 'ByteString's. Note that this attempts to decode
-- using the functions from $decoding, and throws away the result.

validBase16 :: ByteString -> Maybe (AtBase "16")
validBase16 bs = BaseN bs <$ decodeBase16 bs

validBase16Either :: ByteString -> Either String (AtBase "16")
validBase16Either bs = second (const $ BaseN bs) $ decodeBase16Either bs

validBase16upper :: ByteString -> Maybe (AtBase "16u")
validBase16upper bs = BaseN bs <$ hush (decodeBase16upper bs)

validBase16upperEither :: ByteString -> Either String (AtBase "16u")
validBase16upperEither bs = second (const $ BaseN bs) $ decodeBase16upper bs

validBase32hex :: ByteString -> Maybe (AtBase "32x")
validBase32hex bs = BaseN bs <$ hush (decodeBase32hex bs)

validBase32hexEither :: ByteString -> Either String (AtBase "32x")
validBase32hexEither bs = second (const $ BaseN bs) $ decodeBase32hex bs

validBase32hexupper :: ByteString -> Maybe (AtBase "32xu")
validBase32hexupper bs = BaseN bs <$ hush (decodeBase32hexupper bs)

validBase32hexupperEither :: ByteString -> Either String (AtBase "32xu")
validBase32hexupperEither bs = second (const $ BaseN bs) $ decodeBase32hexupper bs

validBase32hexpad :: ByteString -> Maybe (AtBase "32xp")
validBase32hexpad bs = BaseN bs <$ hush (decodeBase32hexpad bs)

validBase32hexpadEither :: ByteString -> Either String (AtBase "32xp")
validBase32hexpadEither bs = second (const $ BaseN bs) $ decodeBase32hexpad bs

validBase32hexpadupper :: ByteString -> Maybe (AtBase "32xpu")
validBase32hexpadupper bs = BaseN bs <$ hush (decodeBase32hexpadupper bs)

validBase32hexpadupperEither :: ByteString -> Either String (AtBase "32xpu")
validBase32hexpadupperEither bs = second (const $ BaseN bs) $ decodeBase32hexpadupper bs

validBase32 :: ByteString -> Maybe (AtBase "32")
validBase32 bs = BaseN bs <$ hush (decodeBase32 bs)

validBase32Either :: ByteString -> Either String (AtBase "32")
validBase32Either bs = second (const $ BaseN bs) $ decodeBase32 bs

validBase32z :: ByteString -> Maybe (AtBase "32z")
validBase32z bs = BaseN bs <$ hush (decodeBase32z bs)

validBase32zEither :: ByteString -> Either String (AtBase "32z")
validBase32zEither bs = second (const $ BaseN bs) $ decodeBase32z bs

validBase32upper :: ByteString -> Maybe (AtBase "32u")
validBase32upper bs = BaseN bs <$ hush (decodeBase32upper bs)

validBase32upperEither :: ByteString -> Either String (AtBase "32u")
validBase32upperEither bs = second (const $ BaseN bs) $ decodeBase32upper bs

validBase32pad :: ByteString -> Maybe (AtBase "32p")
validBase32pad bs = BaseN bs <$ hush (decodeBase32pad bs)

validBase32padEither :: ByteString -> Either String (AtBase "32p")
validBase32padEither bs = second (const $ BaseN bs) $ decodeBase32pad bs

validBase32padupper :: ByteString -> Maybe (AtBase "32pu")
validBase32padupper bs = BaseN bs <$ hush (decodeBase32padupper bs)

validBase32padupperEither :: ByteString -> Either String (AtBase "32pu")
validBase32padupperEither bs = second (const $ BaseN bs) $ decodeBase32padupper bs

validBase58btc :: ByteString -> Maybe (AtBase "58btc")
validBase58btc bs = BaseN bs <$ hush (decodeBase58btc bs)

validBase58btcEither :: ByteString -> Either String (AtBase "58btc")
validBase58btcEither bs = second (const $ BaseN bs) $ decodeBase58btc bs

validBase58flickr :: ByteString -> Maybe (AtBase "58flickr")
validBase58flickr bs = BaseN bs <$ hush (decodeBase58flickr bs)

validBase58flickrEither :: ByteString -> Either String (AtBase "58flickr")
validBase58flickrEither bs = second (const $ BaseN bs) $ decodeBase58flickr bs

validBase64 :: ByteString -> Maybe (AtBase "64")
validBase64 bs = BaseN bs <$ decodeBase64 bs

validBase64Either :: ByteString -> Either String (AtBase "64")
validBase64Either bs = second (const $ BaseN bs) $ decodeBase64Either bs

validBase64pad :: ByteString -> Maybe (AtBase "64p")
validBase64pad bs = BaseN bs <$ hush (decodeBase64pad bs)

validBase64padEither :: ByteString -> Either String (AtBase "64p")
validBase64padEither bs = second (const $ BaseN bs) $ decodeBase64pad bs

validBase64url :: ByteString -> Maybe (AtBase "64url")
validBase64url bs = BaseN bs <$ hush (decodeBase64url bs)

validBase64urlEither :: ByteString -> Either String (AtBase "64url")
validBase64urlEither bs = second (const $ BaseN bs) $ decodeBase64url bs

validBase64urlpad :: ByteString -> Maybe (AtBase "64urlpad")
validBase64urlpad bs = BaseN bs <$ hush (decodeBase64urlpad bs)

validBase64urlpadEither :: ByteString -> Either String (AtBase "64urlpad")
validBase64urlpadEither bs = second (const $ BaseN bs) $ decodeBase64urlpad bs

class KnownSymbol b => ValidBase (b :: Symbol) where
    validAtBase       :: proxy b -> ByteString -> Maybe (AtBase b)
    validAtBaseEither :: proxy b -> ByteString -> Either String (AtBase b)

instance ValidBase "id" where
    validAtBase       = const (pure . BaseN)
    validAtBaseEither = const (pure . BaseN)
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "2" where
    validAtBase       = const (pure . BaseN)
    validAtBaseEither = const (pure . BaseN)
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "16" where
    validAtBase       = const validBase16
    validAtBaseEither = const validBase16Either
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "16u" where
    validAtBase       = const validBase16upper
    validAtBaseEither = const validBase16upperEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "32x" where
    validAtBase       = const validBase32hex
    validAtBaseEither = const validBase32hexEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "32xu" where
    validAtBase       = const validBase32hexupper
    validAtBaseEither = const validBase32hexupperEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "32xp" where
    validAtBase       = const validBase32hexpad
    validAtBaseEither = const validBase32hexpadEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "32xpu" where
    validAtBase       = const validBase32hexpadupper
    validAtBaseEither = const validBase32hexpadupperEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "32" where
    validAtBase       = const validBase32
    validAtBaseEither = const validBase32Either
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "32z" where
    validAtBase       = const validBase32z
    validAtBaseEither = const validBase32zEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "32u" where
    validAtBase       = const validBase32upper
    validAtBaseEither = const validBase32upperEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "32p" where
    validAtBase       = const validBase32pad
    validAtBaseEither = const validBase32padEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "32pu" where
    validAtBase       = const validBase32padupper
    validAtBaseEither = const validBase32padupperEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "58btc" where
    validAtBase       = const validBase58btc
    validAtBaseEither = const validBase58btcEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "58flickr" where
    validAtBase       = const validBase58flickr
    validAtBaseEither = const validBase58flickrEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "64" where
    validAtBase       = const validBase64
    validAtBaseEither = const validBase64Either
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "64p" where
    validAtBase       = const validBase64pad
    validAtBaseEither = const validBase64padEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "64url" where
    validAtBase       = const validBase64url
    validAtBaseEither = const validBase64urlEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

instance ValidBase "64urlpad" where
    validAtBase       = const validBase64urlpad
    validAtBaseEither = const validBase64urlpadEither
    {-# INLINE validAtBase       #-}
    {-# INLINE validAtBaseEither #-}

-- | Like 'validAtBase', but also return the decoded 'ByteString'.
validAndDecoded
    :: DecodeBase b
    => proxy b
    -> ByteString
    -> Maybe (AtBase b, ByteString)
validAndDecoded at bs = (BaseN bs,) <$> decodeAtBase at bs

-- | Like 'validAtBaseEither', but also return the decoded 'ByteString'.
validAndDecodedEither
    :: DecodeBase b
    => proxy b
    -> ByteString
    -> Either String (AtBase b, ByteString)
validAndDecodedEither at bs = (BaseN bs,) <$> decodeAtBaseEither at bs

-- Text ------------------------------------------------------------------------

-- | Like 'encodeAtBase', but from a 'Text' value.
encodedTextAtBase :: Base b -> Text -> AtBase b
encodedTextAtBase b = encodeAtBase b . encodeUtf8
{-# INLINE encodedTextAtBase #-}

-- | Like 'encodedBytes', but returns a 'Text' value.
encodedText :: AtBase b -> Text
encodedText (BaseN bs) = decodeLatin1 bs
{-# INLINE encodedText #-}

-- | Like 'encodedBuilder', but returns a text 'T.Builder'.
encodedTextBuilder :: AtBase b -> T.Builder
encodedTextBuilder = T.fromText . encodedText
{-# INLINE encodedTextBuilder #-}

-- Formatting ------------------------------------------------------------------

-- | Format a base-n encoded value.
format, formatAtBase :: Formatting.Format r (AtBase b -> r)
format = Formatting.later encodedTextBuilder

formatAtBase = format
{-# INLINE formatAtBase #-}

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

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
