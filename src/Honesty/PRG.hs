
module Honesty.PRG(
    ROM,
    init, read,
    bytes,
    ) where

import Data.Array(Array,(!),listArray)
import Prelude hiding (init,read)

import Honesty.Byte

data ROM = ROM { bytes :: [Byte], bytesA :: Array Int Byte }

size :: Int
size = 0x4000 -- 16k

init :: [Byte] -> ROM
init bytes = if
    | n == size -> ROM { bytes, bytesA = listArray (0,size-1) bytes }
    | otherwise -> error $ "PRG.init: " <> show n
    where
        n = length bytes

quick :: Bool
quick = True -- fps: 1.25 --> 15

read :: ROM -> Int -> Byte
read rom a = if
    | inRange a -> if
          | quick -> bytesA rom ! a
          | otherwise -> bytes rom !! a -- pre optimization -- this was terrible!
    | otherwise ->
      error $ "PRG.read: " <> show a

inRange :: Int -> Bool
inRange a = a >= 0 && a < size
