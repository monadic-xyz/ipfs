{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright   : 2018 Monadic GmbH
-- License     : BSD3
-- Maintainer  : kim@monadic.xyz, team@monadic.xyz
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- <https://github.com/ipld/cid Content IDentifiers>
--
-- \"CID is a format for referencing content in distributed information systems,
-- like IPFS. It leverages content addressing, cryptographic hashing, and
-- self-describing formats. It is the core identifier used by IPFS and IPLD.\"
module Data.IPLD.CID
    ( Version (..)
    , Codec (..)

    , CID
    , cidVersion
    , cidCodec
    , cidHash
    , newCidV0
    , newCidV1
    , buildCid
    , decodeCid
    , getCid

    , cidFromText
    , cidToText

    , codecToCode
    , codecFromCode
    )
where

import           Control.DeepSeq (NFData)
import           Control.Monad ((>=>))
import           Crypto.Hash (Digest, SHA256)
import           Data.Bifunctor (bimap)
import           Data.Binary.Get as Binary
import           Data.Binary.VarInt (buildVarInt, getVarInt)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.BaseN as BaseN
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Multibase as Multibase
import           Data.Hashable (Hashable)
import           Data.Multihash (Multihash, Multihashable)
import qualified Data.Multihash as Multihash
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Word (Word8)
import           GHC.Generics (Generic)

-- | Specification version.
data Version = V0 | V1
    deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance Hashable Version
instance NFData   Version

-- | The content type or format of the data being addressed, specified as a
-- <https://github.com/multiformats/multicodec multicodec>.
--
-- Note that we do not currently have a full multicodec implementation, as it is
-- overly complicated for our purposes. We also only support 'Codec's on an
-- as-needed basis. Future versions may utilise a separate library.
data Codec
    = Raw
    | DagProtobuf
    | DagCbor
    | GitRaw
    deriving (Eq, Show, Ord, Generic)

instance Hashable Codec
instance NFData   Codec

-- | A Content IDentifier.
--
-- * 'V0' 'CID's are merely SHA256 hashes, base58-encoded using the bitcoin
-- alphabet. The 'Codec' is implicitly 'DagProtobuf'.
-- * 'V1' 'CID's may use any 'Multihash', and any of the supported 'Codec's.
data CID = CID
    { cidVersion :: Version
    , cidCodec   :: Codec
    , cidHash    :: Multihash
    } deriving (Eq, Ord, Generic)

instance Hashable CID
instance NFData   CID

instance Show CID where
    show = Text.unpack . cidToText

instance Read CID where
    readsPrec _ =
        either (const []) (\cid -> [(cid, "")]) . cidFromText . Text.pack

-- CID -------------------------------------------------------------------------

-- | Create a 'V0' 'CID'.
newCidV0 :: Digest SHA256 -> CID
newCidV0 dig = CID
    { cidVersion = V0
    , cidCodec   = DagProtobuf
    , cidHash    = Multihash.fromDigest dig
    }

-- | Create a 'V1' 'CID'.
newCidV1 :: Multihashable a => Codec -> Digest a -> CID
newCidV1 codec dig = CID
    { cidVersion = V1
    , cidCodec   = codec
    , cidHash    = Multihash.fromDigest dig
    }

-- | Serialise a 'CID'.
buildCid :: CID -> Builder
buildCid CID{..} = case cidVersion of
    V0 -> Builder.byteString (Multihash.encodedBytes cidHash)
    V1 -> buildVarInt (fromEnum cidVersion)
       <> buildVarInt (codecToCode cidCodec)
       <> Builder.byteString (Multihash.encodedBytes cidHash)

-- | Decode a 'CID' from a strict 'ByteString'.
--
-- @
--    decodeCid . buildCid ≡ Right
-- @
decodeCid :: ByteString -> Either String CID
decodeCid bs
  | isV0      = newCidV0 <$> Multihash.decodeDigest bs
  | otherwise = bimap _3 _3 . Binary.runGetOrFail getCid $ LBS.fromStrict bs
  where
    isV0  = BS.length bs == 34 && magic == BS.take 2 bs
    magic = BS.pack [18,32]

    _3 (_,_,x) = x

-- | Deserialise a 'CID' in the 'Binary.Get' monad.
--
-- Note that this does __/not/__ support 'V0' 'CID's.
getCid :: Binary.Get CID
getCid = do
    cidVersion <- do
        v <- Binary.getWord8 >>= getVarInt
        if v < minVersion || v > maxVersion then
            fail $ "CID: Invalid version: " <> show v
        else
            pure $ toEnum v

    case cidVersion of
        V1 -> do
            cidCodec <- do
                c <- Binary.getWord8 >>= getVarInt
                maybe (fail ("CID: Unknown Codec: " <> show c)) pure
                    $ codecFromCode c
            cidHash <- Multihash.getMultihash
            pure CID{..}
        v  -> fail $ "CID: Unsupported version: " <> show v
  where
    maxVersion = fromEnum (maxBound :: Version)
    minVersion = fromEnum (minBound :: Version)

-- | Decode a 'CID' from a textual representation.
--
-- The 'Text' value is expected to be base58 (bitcoin) encoded (for 'V0'
-- 'CID's), or a valid 'Multibase'.
--
-- @
--    cidFromText . cidToText ≡ id
-- @
cidFromText :: Text -> Either String CID
cidFromText t = decodeBase >=> decodeCid $ encodeUtf8 t
  where
    isV0 = Text.length t == 46 && "Qm" `Text.isPrefixOf` t

    decodeBase | isV0      = BaseN.decodeBase58btc
               | otherwise = Multibase.decode >=> guardReserved

    -- "If the first decoded byte is 0x12, return an error. CIDv0 CIDs may not
    -- be multibase encoded and there will be no CIDv18 (0x12 = 18) to prevent
    -- ambiguity with decoded CIDv0s."
    guardReserved bs = case BS.uncons bs of
        Just (x, _) | x == 18 -> Left "CID > V0 starts with reserved byte 0x12"
        _                     -> Right bs

-- | Encode a 'CID' to a textual representation.
--
-- The result is either a base58 (bitcoin) encoded string of just the 'cidHash'
-- value for 'V0' 'CID's, or otherwise a 'Multibase' value at base 'Base58btc'
-- of the binary representation of the 'CID' (as produced by 'buildCid').
cidToText :: CID -> Text
cidToText cid =
      decodeUtf8
    $ case cidVersion cid of
          V0 -> BaseN.encodedBytes
              . BaseN.encodeBase58btc
              . Multihash.encodedBytes
              $ cidHash cid
          V1 -> Multibase.fromMultibase
              . Multibase.encode
              . BaseN.encodeBase58btc
              . LBS.toStrict . Builder.toLazyByteString
              $ buildCid cid

-- Codec -----------------------------------------------------------------------

-- | <https://github.com/multiformats/multicodec multicodec> numerical code of
-- the given 'Codec'.
codecToCode :: Codec -> Word8
codecToCode Raw         = 0x55
codecToCode DagProtobuf = 0x70
codecToCode DagCbor     = 0x71
codecToCode GitRaw      = 0x78

-- | Attempt to convert from a <https://github.com/multiformats/multicodec multicodec>
-- numerical code to a 'Codec'.
codecFromCode :: Word8 -> Maybe Codec
codecFromCode 0x55 = pure Raw
codecFromCode 0x70 = pure DagProtobuf
codecFromCode 0x71 = pure DagCbor
codecFromCode 0x78 = pure GitRaw
codecFromCode _    = Nothing
