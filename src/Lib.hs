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
--import Data.Word16
import Text.Printf (printf)

main :: IO ()
main = do
    bytes <- loadFile "data/nestest.nes"
    let ops = dis $ take 30 $ drop (headerSize + topJunkSize) bytes
    mapM_ putStrLn $ displayOps (startAddr `addAddr` topJunkSize) ops

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

newtype Byte = Byte { unByte :: Word8 } deriving (Eq,Num)

byteOfInt :: Int -> Byte
byteOfInt = Byte . fromIntegral

byteToInt :: Byte -> Int
byteToInt = fromIntegral . unByte

instance Show Byte where show = printf "%02X" . unByte

data Op
    = Unknown Byte
    | JMP Addr -- also indirect mode
    | LDX Mode
    deriving (Show)

displayOps :: Addr -> [Op] -> [String]
displayOps a = \case
    [] -> []
    op:ops -> displayOp a op : displayOps (a `addAddr` size op) ops

displayOp :: Addr -> Op -> String
displayOp a op =
    show a <> "  " <> rjust 8 (showOpBytes op) <> "  " <> show op

rjust :: Int -> String -> String
rjust n s = take (max 0 (n - length s)) $ repeat '.'

showOpBytes :: Op -> String
showOpBytes op = unwords $ map show $ opBytes op

size :: Op -> Int
size = \case
    Unknown _ -> 1
    JMP  _-> 3
    LDX mode -> 1 + sizeMode mode

opBytes :: Op -> [Byte]
opBytes = \case
    Unknown b -> [b]
    JMP a -> 0x4C : addrBytes a
    LDX mode -> 0xA2 : modeBytes mode

dis :: [Byte] -> [Op]
dis = \case
    [] -> []
    b:bs -> case b of
        0x4C -> JMP a : dis bs' where (a,bs') = takeA bs
        0xA2 -> LDX (Immediate b) : dis bs' where (b,bs') = takeB bs
        _ -> Unknown b : dis bs

takeA :: [Byte] -> (Addr,[Byte])
takeA = \case
    lo:hi:rest -> (addrOfLoHi lo hi, rest)
    _ -> error "takeA"

takeB :: [Byte] -> (Byte,[Byte])
takeB = \case
    b:rest -> (b, rest)
    _ -> error "takeB"

data Mode =
    Immediate Byte

sizeMode :: Mode -> Int
sizeMode = \case
    Immediate _ -> 1

modeBytes :: Mode -> [Byte]
modeBytes = \case
    Immediate b -> [b]

instance Show Mode where
    show = \case
        Immediate byte -> "#$" <> show byte

data Addr = Addr { lo :: Byte, hi :: Byte } --deriving (Show)
--data Addr = Addr { unAddr :: Word16 }

instance Show Addr where show Addr{lo,hi} = show hi <> show lo

addrToInt :: Addr -> Int
addrToInt Addr{lo,hi} = byteToInt lo + 256 * byteToInt hi

instance Num Addr where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = addrOfInt . fromIntegral
    negate = undefined

addrOfInt :: Int -> Addr
addrOfInt n = do
    let lo = byteOfInt $ n `mod` 256
    let hi = byteOfInt $ n `div` 256
    Addr {lo,hi}

addrOfLoHi :: Byte -> Byte -> Addr
--addrOfLoHi lo hi = lo + 256 * hi
addrOfLoHi lo hi = Addr {lo,hi}

addAddr :: Addr -> Int -> Addr
addAddr a n = addrOfInt (addrToInt a + n)

addrBytes :: Addr -> [Byte]
addrBytes Addr{lo,hi} = [lo,hi]
