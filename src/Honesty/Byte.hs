
module Honesty.Byte(
    Byte(..),
    byteOfInt, byteToUnsigned, byteToSigned, bytesToString, adc,
    ) where

import Data.Bits (Bits)
import Data.Word8 (Word8)
import GHC.Arr (Ix)
import Text.Printf (printf)
import qualified Data.Char as Char (chr)

newtype Byte = Byte { unByte :: Word8 } deriving (Eq,Ord,Enum,Num,Bits,Ix)

instance Show Byte where show = printf "%02X" . unByte

byteOfInt :: Int -> Byte
byteOfInt = Byte . fromIntegral

byteToUnsigned :: Byte -> Int
byteToUnsigned = fromIntegral . unByte

byteToSigned :: Byte -> Int
byteToSigned = sign . fromIntegral . unByte
    where sign n = if n>=128 then n-256 else n

bytesToString :: [Byte] -> String
bytesToString = map (Char.chr .byteToSigned)

adc :: Bool -> Byte -> Byte -> (Byte,Bool)
adc cin x y = (Byte $ fromIntegral res,cout) where
    res = byteToUnsigned x + byteToUnsigned y + (if cin then 1 else 0)
    cout = res >= 256
