
module Six502.Values(
    Addr(..),
    addAddr,
    minusAddr, zeroPageAddr, page1Addr,
    Byte(..),
    byteOfInt, byteToUnsigned, byteToSigned, adc,
    addrOfHiLo, addrToHiLo,
    ) where

import Data.Bits
import Data.Word (Word16)
import Data.Word8 (Word8)
import Text.Printf (printf)

newtype Addr = Addr { unAddr :: Word16 } deriving (Eq,Ord,Num)

instance Show Addr where show = printf "%04X" . unAddr

addAddr :: Addr -> Int -> Addr
addAddr a n = Addr (unAddr a + fromIntegral n)

minusAddr :: Addr -> Addr -> Int
minusAddr later earlier = fromIntegral (unAddr later - unAddr earlier)

zeroPageAddr :: Byte -> Addr
zeroPageAddr b = Addr $ fromIntegral $ byteToUnsigned b

page1Addr :: Byte -> Addr
page1Addr b = Addr $ 256 + (fromIntegral $ byteToUnsigned b)

newtype Byte = Byte { unByte :: Word8 } deriving (Eq,Ord,Enum,Num,Bits)

instance Show Byte where show = printf "%02X" . unByte

byteOfInt :: Int -> Byte
byteOfInt = Byte . fromIntegral

byteToUnsigned :: Byte -> Int
byteToUnsigned = fromIntegral . unByte

byteToSigned :: Byte -> Int
byteToSigned = sign . fromIntegral . unByte
    where sign n = if n>=128 then n-256 else n

adc :: Bool -> Byte -> Byte -> (Byte,Bool)
adc cin x y = (Byte $ fromIntegral res,cout) where
    res = byteToUnsigned x + byteToUnsigned y + (if cin then 1 else 0)
    cout = res >= 256

addrOfHiLo :: Byte -> Byte -> Addr
addrOfHiLo hi lo =
    Addr (256 * fromIntegral (unByte hi) + fromIntegral (unByte lo))

addrToHiLo :: Addr -> (Byte,Byte)
addrToHiLo a = (hi,lo) where -- do it with Bits instead?
    lo = byteOfInt $ n `mod` 256
    hi = byteOfInt $ n `div` 256
    n = fromIntegral $ unAddr a
