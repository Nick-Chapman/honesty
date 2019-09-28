
module PRG(
    ROM, unROM, init, read,
    ) where

import Prelude hiding (init,read)
import Six502.Values

newtype ROM = ROM { unROM :: [Byte] }

size :: Int
size = 0x4000

init :: [Byte] -> ROM
init bytes = if
    | n == size -> ROM bytes
    | otherwise -> error $ "PRG.init: " <> show n
    where
        n = length bytes

read :: ROM -> Int -> Byte
read (ROM bytes) a = if
    | inRange a -> bytes !! a -- TODO: optimize! -- this is terrible!
    | otherwise -> error $ "PRG.read: " <> show a

inRange :: Int -> Bool
inRange a = a >= 0 && a < size
