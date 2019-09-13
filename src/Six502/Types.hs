
module Six502.Types(
    Instruction(..),
    Mode(..),
    Byte(..),
    ) where

import Data.Bits
import Data.Word8 (Word8)
import Text.Printf (printf)

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

newtype Byte = Byte { unByte :: Word8 } deriving (Eq,Ord,Num,Bits)

instance Show Byte where show = printf "%02X" . unByte
