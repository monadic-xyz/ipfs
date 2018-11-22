{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

-- |
-- Copyright   : 2018 Monadic GmbH
-- License     : BSD3
-- Maintainer  : kim@monadic.xyz, team@monadic.xyz
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- A mapping of <https://github.com/multiformats/multihash multihash> to CBOR.
--
-- Instead of an opaque, multihash-encoded blob, this provides a tagged CBOR
-- encoding where the hash algorithm used is a 'Word16' field containing the
-- multihash code for the algorithm, followed by the digest bytes.
module Codec.Serialise.Multihash
    ( encode
    , decode

    -- * Re-exports
    , Multihashable
    )
where

import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Applicative (liftA2)
import qualified Crypto.Hash as C
import           Data.ByteArray (convert)
import           Data.Multihash (Multihashable)
import qualified Data.Multihash as Multi
import           Data.Proxy (Proxy(..))

-- | Multihash-like CBOR 'CBOR.Encoding' of a 'C.Digest'.
encode :: Multihashable a => C.Digest a -> CBOR.Encoding
encode dig =
       CBOR.encodeListLen 3
    <> CBOR.encodeWord 0    -- constructor tag / version
    <> CBOR.encode (Multi.toCode (Multi.fromCryptonite dig))
    <> CBOR.encodeBytes (convert dig)

-- | CBOR 'CBOR.Decoder' to decode a 'C.Digest' encoded via 'encodeCBOR'.
decode :: forall s a. Multihashable a => CBOR.Decoder s (C.Digest a)
decode = do
    pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
    case pre of
        (3, 0) -> do
            rithm <- Multi.fromCode <$> CBOR.decode
            case rithm of
                Nothing -> fail "Unknown HashAlgorithm"
                Just  a
                    | Multi.fromCryptonite (Proxy @a) /= a ->
                        fail "Algorithm Mismatch"
                    | otherwise -> do
                        bytes <- CBOR.decodeBytes
                        maybe (fail "Invalid Digest") pure
                            $ C.digestFromByteString bytes

        _ -> fail "Multihash: Invalid Tag"
