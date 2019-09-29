
module Six502.Disassembler (
    displayOpLine,
    displayOpLines,
    ljust,
    ) where

import Data.Char as Char(chr)

import Addr
import Byte
import Six502.Operations
import Six502.Decode (opBytes,opSize)

displayOpLines :: Addr -> [Op] -> [String]
displayOpLines a = \case
    [] -> []
    op:ops ->
        displayOpLine a op : displayOpLines (a `addAddr` opSize op) ops

displayOpLine :: Addr -> Op -> String
displayOpLine at op = show at <> "  " <> case op of
    Unknown byte ->
        ljust 8 (showOpBytes op)
        <> "  ??? " <> show [Char.chr $ fromIntegral $ unByte byte]
    Op instruction mode rand ->
        ljust 8 (showOpBytes op)
        <> (if unofficial instruction then " *" else "  ")
        <> showInstruction instruction
        <> displayArg at (mode,rand)

showOpBytes :: Op -> String
showOpBytes op = unwords $ map show $ opBytes op

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

showInstruction :: Instruction -> String
showInstruction = \case
    SBC_extra -> "SBC"
    instruction -> if unofficialNop instruction then "NOP" else show instruction

displayArg :: Addr -> (Mode,Arg) -> String
displayArg at = \case
    (Immediate,ArgByte b) -> " #$" <> show b
    (ZeroPage,ArgByte b) -> " $" <> show b
    (Relative,ArgByte b) -> " $" <> show (at `addAddr` (2 + byteToSigned b))
    (Absolute,ArgAddr a) -> " $" <> show a
    (Implied,ArgNull) -> ""
    (Accumulator,ArgNull) -> " A"
    (IndexedIndirect,ArgByte b) -> " ($" <> show b <> ",X)"
    (IndirectIndexed,ArgByte b) -> " ($" <> show b <> "),Y"
    (Indirect,ArgAddr a) -> " ($" <> show a <> ")"
    (ZeroPageX,ArgByte b) -> " $" <> show b <> ",X"
    (ZeroPageY,ArgByte b) -> " $" <> show b <> ",Y"
    (AbsoluteX,ArgAddr a) -> " $" <> show a <> ",X"
    (AbsoluteY,ArgAddr a) -> " $" <> show a <> ",Y"
    x -> error $ "displayArg: " <> show x

unofficial :: Instruction -> Bool
unofficial i = i `elem` [DCP,ISB,LAX,RLA,RRA,SAX,SLO,SRE,SBC_extra] || unofficialNop i

unofficialNop :: Instruction -> Bool
unofficialNop i = i `elem`
    [NOP_04,NOP_44,NOP_64
    ,NOP_0C
    ,NOP_14,NOP_34,NOP_54,NOP_74,NOP_D4,NOP_F4
    ,NOP_1A,NOP_3A,NOP_5A,NOP_7A,NOP_DA,NOP_FA
    ,NOP_80
    ,NOP_1C,NOP_3C,NOP_5C,NOP_7C,NOP_DC,NOP_FC
    ]
