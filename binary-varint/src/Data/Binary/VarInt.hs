-- |
-- Copyright   : 2018 Monadic GmbH
-- License     : BSD3
-- Maintainer  : kim@monadic.xyz, team@monadic.xyz
-- Stability   : stable
-- Portability : portable
--
module Data.Binary.VarInt
    ( getVarInt
    , putVarInt
    , buildVarInt
    )
where

import qualified Data.Binary as Binary
import           Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as Builder
import qualified Data.Binary.Put as Binary (putLazyByteString)
import           Data.Bits
import           Data.Word (Word8)

getVarInt :: (Num a, Bits a) => Word8 -> Binary.Get a
getVarInt n
    | testBit n 7 = do
        v <- Binary.getWord8 >>= getVarInt
        pure $ shiftL v 7 .|. clearBit (fromIntegral n) 7
    | otherwise = pure $ fromIntegral n

putVarInt :: (Integral a, Bits a) => a -> Binary.Put
putVarInt = Binary.putLazyByteString . Builder.toLazyByteString . buildVarInt

buildVarInt :: (Integral a, Bits a) => a -> Builder
buildVarInt n
    | n < 0x80  = Builder.singleton $ fromIntegral n
    | otherwise = Builder.singleton (setBit (fromIntegral n) 7)
               <> buildVarInt (shiftR n 7)
