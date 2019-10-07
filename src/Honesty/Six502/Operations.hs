
module Honesty.Six502.Operations(
    Op(..),
    Instruction(..),
    Mode(..),
    Arg(..)
    ) where

import Honesty.Addr(Addr)
import Honesty.Byte(Byte)

data Op = Unknown Byte | Op Instruction Mode Arg deriving (Show)

data Instruction
    = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI
    | BNE | BPL | BRK | BVC | BVS | CLC | CLD | CLI
    | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR
    | INC | INX | INY | JMP | JSR | LDA | LDX | LDY
    | LSR | NOP | ORA | PHA | PHP | PLA | PLP | ROL
    | ROR | RTI | RTS | SBC | SEC | SED | SEI | STA
    | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
    | SBC_extra
    | DCP | ISB | LAX | RLA | RRA | SAX | SLO | SRE
    | NOP_04 | NOP_44 | NOP_64
    | NOP_0C
    | NOP_14 | NOP_34 | NOP_54 | NOP_74 | NOP_D4 | NOP_F4
    | NOP_1A | NOP_3A | NOP_5A | NOP_7A | NOP_DA | NOP_FA
    | NOP_80
    | NOP_1C | NOP_3C | NOP_5C | NOP_7C | NOP_DC | NOP_FC
    deriving (Eq,Show)

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

data Arg = ArgByte Byte | ArgAddr Addr | ArgNull deriving (Show)
