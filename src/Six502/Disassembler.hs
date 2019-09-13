
module Six502.Disassembler (
    displayOpLine,
    displayOpLines,
    ljust,
    ) where

import Data.Char as Char(chr)

import Six502.Values
import Six502.Operations
import Six502.Decode (opBytes,opSize)

displayOpLines :: Addr -> [Op] -> [String]
displayOpLines a = \case
    [] -> []
    op:ops ->
        displayOpLine a op : displayOpLines (a `addAddr` opSize op) ops

displayOpLine :: Addr -> Op -> String
displayOpLine at op = show at <> "  " <> case op of
    Unknown bytes ->
        ljust 8 (showOpBytes op)
        <> "  ??? " <> show (map (Char.chr . fromIntegral . unByte) bytes)
    Op instruction mode rand ->
        ljust 8 (showOpBytes op)
        <> (if unofficial instruction then " *" else "  ")
        <> showInstruction instruction
        <> displayRand at (mode,rand)

showOpBytes :: Op -> String
showOpBytes op = unwords $ map show $ opBytes op

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

showInstruction :: Instruction -> String
showInstruction = \case
    SBC_extra -> "SBC"
    instruction -> if unofficialNop instruction then "NOP" else show instruction

displayRand :: Addr -> (Mode,Rand) -> String
displayRand at = \case
    (Immediate,RandByte b) -> " #$" <> show b
    (ZeroPage,RandByte b) -> " $" <> show b
    (Relative,RandByte b) -> " $" <> show (at `addAddr` (2 + byteToSigned b))
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
