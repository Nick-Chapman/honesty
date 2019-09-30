
module CHR(
    ROM,
    init, read,
    bytes,
    ) where

import Data.Array(Array,(!),listArray)
import Prelude hiding (init,read)

import Byte

data ROM = ROM { bytes :: [Byte], bytesA :: Array Int Byte }

size :: Int
size = 0x2000 -- 8k

init :: [Byte] -> ROM
init bytes = if
    | n == size -> ROM { bytes, bytesA = listArray (0,size-1) bytes }
    | otherwise -> error $ "CHR.init: " <> show n
    where
        n = length bytes

read :: ROM -> Int -> Byte
read rom a = if
    | inRange a -> bytesA rom ! a
    | otherwise ->
      error $ "CHR.read: " <> show a

inRange :: Int -> Bool
inRange a = a >= 0 && a < size
