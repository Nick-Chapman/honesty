{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (main) where

import qualified Data.ByteString as BS
import Data.Word8
import Data.Word
import Text.Printf (printf)
import Control.Monad

main :: IO ()
main = do
    bytesIncHeaderAndTopJunk <- loadFile "data/nestest.nes"
    let bytes = drop (headerSize + topJunkSize) bytesIncHeaderAndTopJunk
    let ops = dis bytes
    let bytes' = reAssemble ops
    --putStrLn $ unwords $ map show $ take 10 bytes
    --putStrLn $ unwords $ map show $ take 10 bytes'
    when (bytes /= bytes') $ fail "re-assemble failed"
    mapM_ putStrLn $ displayOps (startAddr `addAddr` topJunkSize) (take 20 ops)

startAddr :: Addr
startAddr = 0xC000

headerSize :: Int
headerSize = 16

topJunkSize :: Int
topJunkSize = 0x5F5

loadFile :: String -> IO [Byte]
loadFile path = do
    bs :: BS.ByteString <- BS.readFile path
    let ws :: [Word8] = BS.unpack bs
    return $ map Byte ws

reAssemble :: [Op] -> [Byte]
reAssemble = join . map opBytes

displayOps :: Addr -> [Op] -> [String]
displayOps a = \case
    [] -> []
    op:ops -> displayOp a op : displayOps (a `addAddr` size op) ops

displayOp :: Addr -> Op -> String
displayOp a op =
    show a <> "  " <> ljust 8 (showOpBytes op) <> "  " <> show op

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

showOpBytes :: Op -> String
showOpBytes op = unwords $ map show $ opBytes op

data Op
    = Unknown [Byte]
    | JMP Addr -- also indirect mode
    | LDX Mode
    | STX Mode
    | JSR Mode
    deriving (Show)

size :: Op -> Int
size = \case
    Unknown xs -> length xs
    JMP  _-> 3
    LDX mode -> 1 + sizeMode mode
    STX mode -> 1 + sizeMode mode
    JSR mode -> 1 + sizeMode mode

opBytes :: Op -> [Byte]
opBytes = \case
    Unknown bs -> bs
    JMP a -> 0x4C : addrBytes a
    LDX mode -> 0xA2 : modeBytes mode
    STX mode -> 0x86 : modeBytes mode
    JSR mode -> 0x20 : modeBytes mode


dis :: [Byte] -> [Op]
dis = disJ []

disJ :: [Byte] -> [Byte] -> [Op]
disJ junk = \case
    [] -> flush []
    b:bs -> case b of
        0x4C -> flush $ JMP a : dis bs' where (a,bs') = takeA bs
        0xA2 -> flush $ LDX (Immediate b) : dis bs' where (b,bs') = takeB bs
        0x86 -> flush $ STX (PageZero b) : dis bs' where (b,bs') = takeB bs
        0x20 -> flush $ JSR (Absolute a) : dis bs' where (a,bs') = takeA bs
        _ -> disJ (b:junk) bs
    where
        flush :: [Op] -> [Op]
        flush ops = case junk of
            [] -> ops
            _ -> Unknown (reverse junk) : ops

takeA :: [Byte] -> (Addr,[Byte])
takeA = \case
    lo:hi:rest -> (addrOfHiLo hi lo, rest)
    _ -> error "takeA"

takeB :: [Byte] -> (Byte,[Byte])
takeB = \case
    b:rest -> (b, rest)
    _ -> error "takeB"

data Mode
    = Immediate Byte
    | PageZero Byte
    | Absolute Addr

sizeMode :: Mode -> Int
sizeMode = \case
    Immediate _ -> 1
    PageZero _ -> 1
    Absolute _ -> 1 -- BUG

modeBytes :: Mode -> [Byte]
modeBytes = \case
    Immediate b -> [b]
    PageZero b -> [b]
    Absolute a -> addrBytes a

instance Show Mode where
    show = \case
        Immediate b -> "#$" <> show b
        PageZero b -> "$" <> show b
        Absolute a -> "$" <> show a

newtype Addr = Addr { unAddr :: Word16 } deriving (Num)

instance Show Addr where show = printf "%04X" . unAddr

addrOfHiLo :: Byte -> Byte -> Addr
addrOfHiLo hi lo =
    Addr (256 * fromIntegral (unByte hi) + fromIntegral (unByte lo))

addAddr :: Addr -> Int -> Addr
addAddr a n = Addr (unAddr a + fromIntegral n)

addrBytes :: Addr -> [Byte]
addrBytes a = [lo,hi] where -- do it with Bits instead?
    lo = byteOfInt $ n `mod` 256
    hi = byteOfInt $ n `div` 256
    n = fromIntegral $ unAddr a


newtype Byte = Byte { unByte :: Word8 } deriving (Eq,Num)

byteOfInt :: Int -> Byte
byteOfInt = Byte . fromIntegral

instance Show Byte where show = printf "%02X" . unByte
