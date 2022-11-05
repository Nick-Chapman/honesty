
module Honesty.Addr(
    Addr(..),
    addAddr,
    minusAddr, zeroPageAddr, page1Addr,
    addrOfHiLo, addrToHiLo,
    ) where

import Data.Word (Word16)
import Honesty.Byte (Byte,byteToUnsigned,unByte,byteOfInt)
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

addrOfHiLo :: Byte -> Byte -> Addr
addrOfHiLo hi lo =
    Addr (256 * fromIntegral (unByte hi) + fromIntegral (unByte lo))

addrToHiLo :: Addr -> (Byte,Byte)
addrToHiLo a = (hi,lo) where -- do it with Bits instead?
    lo = byteOfInt $ n `mod` 256
    hi = byteOfInt $ n `div` 256
    n = fromIntegral $ unAddr a
