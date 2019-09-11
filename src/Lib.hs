{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Lib (main) where

import qualified Data.ByteString as BS
import Data.Word8
import Data.Word
import Text.Printf (printf)
import Control.Monad
import Data.Char as Char
import Data.Maybe

main :: IO ()
main = do
    bytesIncHeaderAndJunk <- loadFile "data/nestest.nes"
    let bytes = take sizeCode $ drop headerSize bytesIncHeaderAndJunk
    let ops = dis bytes
    let bytes' = reAssemble ops
    when (bytes /= bytes') $ fail "re-assemble failed"
    mapM_ putStrLn $ displayInstrLines (startAddr `addAddr` 0) ops

startAddr :: Addr
startAddr = 0xC000

headerSize :: Int
headerSize = 16

sizeCode :: Int
sizeCode = 0x3B78

loadFile :: String -> IO [Byte]
loadFile path = do
    bs :: BS.ByteString <- BS.readFile path
    let ws :: [Word8] = BS.unpack bs
    return $ map Byte ws

reAssemble :: [Instr] -> [Byte]
reAssemble = join . map instrBytes

displayInstrLines :: Addr -> [Instr] -> [String]
displayInstrLines a = \case
    [] -> []
    instr:instrs ->
        displayInstrLine a instr : displayInstrLines (a `addAddr` size instr) instrs

displayInstrLine :: Addr -> Instr -> String
displayInstrLine at instr = show at <> "  " <> case instr of
    Unknown bytes ->
        ljust 8 (showInstrBytes instr)
        <> " ??? " <> show (map (Char.chr . fromIntegral . unByte) bytes) <> " ???"
    Instr op mode rand ->
        ljust 8 (showInstrBytes instr)
        <> (if unofficial op then " *" else "  ")
        <> show op
        <> displayRand at (mode,rand)

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

showInstrBytes :: Instr -> String
showInstrBytes instr = unwords $ map show $ instrBytes instr

data Op
    = ADC
    | AND
    | ASL
    | BCC
    | BCS
    | BEQ
    | BIT
    | BMI
    | BNE
    | BPL
    | BRK
    | BVC
    | BVS
    | CLC
    | CLD
    | CLI
    | CLV
    | CMP
    | CPX
    | CPY
    | DEC
    | DEX
    | DEY
    | EOR
    | INC
    | INX
    | INY
    | JMP
    | JSR
    | LDA
    | LDX
    | LDY
    | LSR
    | NOP
    | ORA
    | PHA
    | PHP
    | PLA
    | PLP
    | ROL
    | ROR
    | RTI
    | RTS
    | SBC
    | SEC
    | SED
    | SEI
    | STA
    | STX
    | STY
    | TAX
    | TAY
    | TSX
    | TXA
    | TXS
    | TYA

    | LAX

    deriving (Eq,Show)

table :: [(Op,Mode,Byte)]
table =
    [ (ADC, Immediate, 0x69)
    , (ADC, Absolute, 0x6d)
    , (ADC, AbsoluteX, 0x7d)
    , (ADC, AbsoluteY, 0x79)
    , (ADC, ZeroPage, 0x65)
    , (ADC, ZeroPageX, 0x75)
    , (ADC, IndexedIndirect, 0x61)
    , (ADC, IndirectIndexed, 0x71)
    , (AND, Immediate, 0x29)
    , (AND, Absolute, 0x2d)
    , (AND, AbsoluteX, 0x3d)
    , (AND, AbsoluteY, 0x39)
    , (AND, ZeroPage, 0x25)
    , (AND, ZeroPageX, 0x35)
    , (AND, IndexedIndirect, 0x21)
    , (AND, IndirectIndexed, 0x31)
    , (ASL, Accumulator, 0x0a)
    , (ASL, Absolute, 0x0e)
    , (ASL, AbsoluteX, 0x1e)
    , (ASL, ZeroPage, 0x06)
    , (ASL, ZeroPageX, 0x16)
    , (BCC, Relative, 0x90)
    , (BCS, Relative, 0xb0)
    , (BEQ, Relative, 0xf0)
    , (BIT, Absolute, 0x2c)
    , (BIT, ZeroPage, 0x24)
    , (BMI, Relative, 0x30)
    , (BNE, Relative, 0xd0)
    , (BPL, Relative, 0x10)
    , (BRK, Implied, 0x00)
    , (BVC, Relative, 0x50)
    , (BVS, Relative, 0x70)
    , (CLC, Implied, 0x18)
    , (CLD, Implied, 0xd8)
    , (CLI, Implied, 0x58)
    , (CLV, Implied, 0xb8)
    , (CMP, Immediate, 0xc9)
    , (CMP, Absolute, 0xcd)
    , (CMP, AbsoluteX, 0xdd)
    , (CMP, AbsoluteY, 0xd9)
    , (CMP, ZeroPage, 0xc5)
    , (CMP, ZeroPageX, 0xd5)
    , (CMP, IndexedIndirect, 0xc1)
    , (CMP, IndirectIndexed, 0xd1)
    , (CPX, Immediate, 0xe0)
    , (CPX, Absolute, 0xec)
    , (CPX, ZeroPage, 0xe4)
    , (CPY, Immediate, 0xc0)
    , (CPY, Absolute, 0xcc)
    , (CPY, ZeroPage, 0xc4)
    , (DEC, Absolute, 0xce)
    , (DEC, AbsoluteX, 0xde)
    , (DEC, ZeroPage, 0xc6)
    , (DEC, ZeroPageX, 0xd6)
    , (DEX, Implied, 0xca)
    , (DEY, Implied, 0x88)
    , (EOR, Immediate, 0x49)
    , (EOR, Absolute, 0x4d)
    , (EOR, AbsoluteX, 0x5d)
    , (EOR, AbsoluteY, 0x59)
    , (EOR, ZeroPage, 0x45)
    , (EOR, ZeroPageX, 0x55)
    , (EOR, IndexedIndirect, 0x41)
    , (EOR, IndirectIndexed, 0x51)
    , (INC, Absolute, 0xee)
    , (INC, AbsoluteX, 0xfe)
    , (INC, ZeroPage, 0xe6)
    , (INC, ZeroPageX, 0xf6)
    , (INX, Implied, 0xe8)
    , (INY, Implied, 0xc8)
    , (JMP, Absolute, 0x4c)
    , (JMP, Indirect, 0x6c)
    , (JSR, Absolute, 0x20)
    , (LDA, Immediate, 0xa9)
    , (LDA, Absolute, 0xad)
    , (LDA, AbsoluteX, 0xbd)
    , (LDA, AbsoluteY, 0xb9)
    , (LDA, ZeroPage, 0xa5)
    , (LDA, ZeroPageX, 0xb5)
    , (LDA, IndexedIndirect, 0xa1)
    , (LDA, IndirectIndexed, 0xb1)
    , (LDX, Immediate, 0xa2)
    , (LDX, Absolute, 0xae)
    , (LDX, AbsoluteY, 0xbe)
    , (LDX, ZeroPage, 0xa6)
    , (LDX, ZeroPageY, 0xb6)
    , (LDY, Immediate, 0xa0)
    , (LDY, Absolute, 0xac)
    , (LDY, AbsoluteX, 0xbc)
    , (LDY, ZeroPage, 0xa4)
    , (LDY, ZeroPageX, 0xb4)
    , (LSR, Accumulator, 0x4a)
    , (LSR, Absolute, 0x4e)
    , (LSR, AbsoluteX, 0x5e)
    , (LSR, ZeroPage, 0x46)
    , (LSR, ZeroPageX, 0x56)
    , (NOP, Implied, 0xea)
    , (ORA, Immediate, 0x09)
    , (ORA, Absolute, 0x0d)
    , (ORA, AbsoluteX, 0x1d)
    , (ORA, AbsoluteY, 0x19)
    , (ORA, ZeroPage, 0x05)
    , (ORA, ZeroPageX, 0x15)
    , (ORA, IndexedIndirect, 0x01)
    , (ORA, IndirectIndexed, 0x11)
    , (PHA, Implied, 0x48)
    , (PHP, Implied, 0x08)
    , (PLA, Implied, 0x68)
    , (PLP, Implied, 0x28)
    , (ROL, Accumulator, 0x2a)
    , (ROL, Absolute, 0x2e)
    , (ROL, AbsoluteX, 0x3e)
    , (ROL, ZeroPage, 0x26)
    , (ROL, ZeroPageX, 0x36)
    , (ROR, Accumulator, 0x6a)
    , (ROR, Absolute, 0x6e) -- emulator101 had bytes for this and next swapped
    , (ROR, AbsoluteX, 0x7e)
    , (ROR, ZeroPage, 0x66)
    , (ROR, ZeroPageX, 0x76)
    , (RTI, Implied, 0x40)
    , (RTS, Implied, 0x60)
    , (SBC, Immediate, 0xe9)
    , (SBC, Absolute, 0xed)
    , (SBC, AbsoluteX, 0xfd)
    , (SBC, AbsoluteY, 0xf9)
    , (SBC, ZeroPage, 0xe5)
    , (SBC, ZeroPageX, 0xf5)
    , (SBC, IndexedIndirect, 0xe1)
    , (SBC, IndirectIndexed, 0xf1)
    , (SEC, Implied, 0x38)
    , (SED, Implied, 0xf8)
    , (SEI, Implied, 0x78)
    , (STA, Absolute, 0x8d)
    , (STA, AbsoluteX, 0x9d)
    , (STA, AbsoluteY, 0x99)
    , (STA, ZeroPage, 0x85)
    , (STA, ZeroPageX, 0x95)
    , (STA, IndexedIndirect, 0x81)
    , (STA, IndirectIndexed, 0x91)
    , (STX, Absolute, 0x8e)
    , (STX, ZeroPage, 0x86)
    , (STX, ZeroPageY, 0x96)
    , (STY, Absolute, 0x8c)
    , (STY, ZeroPage, 0x84)
    , (STY, ZeroPageX, 0x94)
    , (TAX, Implied, 0xaa)
    , (TAY, Implied, 0xa8)
    , (TSX, Implied, 0xba)
    , (TXA, Implied, 0x8a)
    , (TXS, Implied, 0x9a)
    , (TYA, Implied, 0x98)

    , (LAX, IndexedIndirect, 0xA3)
    ]

unofficial :: Op -> Bool
unofficial op = op `elem` [LAX]

decodeViaTable :: Byte -> Maybe (Op,Mode)
decodeViaTable byte = -- TODO: be more efficient!
    case mapMaybe (\(o,m,b) -> if byte==b then Just (o,m) else Nothing) table of
        [] -> Nothing
        [om] -> Just om
        oms -> error $ "decodeViaTable:" <> show byte <> " -> " <> show oms

encodeViaTable :: (Op,Mode) -> Byte
encodeViaTable (op,mode) = -- TODO: be more efficient!
    case mapMaybe (\(o,m,b) -> if op==o && mode==m then Just b else Nothing) table of
        [] -> error $ "encodeViaTable" <> show (op,mode)
        [b] -> b
        bs -> error $ "encodeViaTable:" <> show (op,mode) <> " -> " <> show bs

data Instr
    = Unknown [Byte]
    | Instr Op Mode Rand

size :: Instr -> Int
size = \case
    Unknown xs -> length xs
    Instr _ mode _ -> 1 + sizeMode mode

instrBytes :: Instr -> [Byte]
instrBytes = \case
    Unknown bs -> bs
    Instr op mode rand -> encodeViaTable (op,mode) : randBytes rand

dis :: [Byte] -> [Instr]
dis = disJ []

disJ :: [Byte] -> [Byte] -> [Instr]
disJ junk = \case
    [] -> flush []
    b:bs ->
        case decodeViaTable b of
            Nothing -> disJ (b:junk) bs
            Just (op,mode) -> flush $ Instr op mode rand : dis bs' where (rand,bs') = takeMode mode bs
    where
        flush :: [Instr] -> [Instr]
        flush instrs = case junk of
            [] -> instrs
            _ -> Unknown (reverse junk) : instrs

data Rand = RandByte Byte | RandAddr Addr | RandNull
    deriving (Show)

randBytes :: Rand -> [Byte]
randBytes = \case
    RandByte b -> [b]
    RandAddr a -> addrBytes a
    RandNull -> []

displayRand :: Addr -> (Mode,Rand) -> String
displayRand at = \case
    (Immediate,RandByte b) -> " #$" <> show b
    (ZeroPage,RandByte b) -> " $" <> show b
    (Relative,RandByte b) -> " $" <> show (at `addAddr` (2 + byteToInt b)) -- TODO: signed for backwards jumps
    (Absolute,RandAddr a) -> " $" <> show a
    (Implied,RandNull) -> ""
    (Accumulator,RandNull) -> " A"
    (IndexedIndirect,RandByte b) -> " ($" <> show b <> ",X)"
    (IndirectIndexed,RandByte b) -> " ($" <> show b <> "),Y"
    (Indirect,RandAddr a) -> " ($" <> show a <> ")"
    (ZeroPageX,RandByte b) -> " $" <> show b <> ",X"
    (ZeroPageY,RandByte b) -> " $" <> show b <> ",Y"
    (AbsoluteX,RandAddr a) -> " $" <> show a <> ",X"
    (AbsoluteY,RandAddr a) -> " $" <> show a <> ",Y"
    x -> error $ "displayRand: " <> show x

data Mode
    = Immediate
    | ZeroPage
    | Absolute
    | Implied
    | Relative
    | AbsoluteX
    | AbsoluteY
    | ZeroPageX
    | ZeroPageY
    | IndexedIndirect
    | IndirectIndexed
    | Accumulator
    | Indirect
    deriving (Eq,Show)

sizeMode :: Mode -> Int
sizeMode = fst . specMode

takeMode :: Mode -> [Byte] -> (Rand,[Byte])
takeMode = snd . specMode

type ModeSpec = (Int, [Byte] -> (Rand,[Byte]))

specMode :: Mode -> ModeSpec
specMode = \case
    Immediate -> spec1
    ZeroPage -> spec1
    Relative -> spec1
    Absolute -> spec2
    Implied -> spec0
    AbsoluteX -> spec2
    AbsoluteY -> spec2
    ZeroPageX -> spec1
    ZeroPageY -> spec1
    IndexedIndirect -> spec1
    IndirectIndexed -> spec1
    Accumulator -> spec0
    Indirect -> spec2

spec0 :: ModeSpec
spec0 = (0, \bs -> (RandNull, bs))

spec1 :: ModeSpec
spec1 = (1, \case b:rest -> (RandByte b, rest); _ -> error "spec1")

spec2 :: ModeSpec
spec2 = (2, \case lo:hi:rest -> (RandAddr $ addrOfHiLo hi lo, rest); _ -> error "take2")

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

byteToInt :: Byte -> Int
byteToInt = fromIntegral . unByte -- TODO: signed! -128..127

instance Show Byte where show = printf "%02X" . unByte
