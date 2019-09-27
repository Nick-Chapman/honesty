
module Six502.Mem( -- address space mapping for the CPU
    Effect(..), reads,
    decode, Decode(..),
    ) where

--import Data.Bits(testBit)
import Prelude hiding(reads)
import Control.Monad (ap,liftM)

import Six502.Values
import qualified PPU.Regs

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

reads :: Addr -> Effect [Byte]
reads addr = do  -- dislike this multi Reads interface now. only use for max of 3
    byte0 <- Read addr
    byte1 <- Read $ addr `addAddr` 1
    byte2 <- Read $ addr `addAddr` 2
    return [byte0,byte1,byte2]

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Addr -> Effect Byte
    Write :: Addr -> Byte -> Effect ()

decode :: String -> Addr -> Decode
decode tag a = if
    | a < 0x800 -> Ram $ fromIntegral $ unAddr a

    -- 3 mirrors -- see if they are ever used...
    -- so far only touch the first two addresses in the first mirror, but never read/write them
    -- | a == 0x800 -> Ram undefined
    -- | a == 0x801 -> Ram undefined

    -- | a < 0x1000 -> Ram $ a `minusAddr` 0x800

    | a == 0x2000 -> PPU PPU.Regs.Control
    | a == 0x2001 -> PPU PPU.Regs.Mask
    | a == 0x2002 -> PPU PPU.Regs.Status
    | a == 0x2003 -> PPU PPU.Regs.OAMADDR
    | a == 0x2005 -> IgnoreFineScrollWrite
    | a == 0x2006 -> PPU PPU.Regs.PPUADDR
    | a == 0x2007 -> PPU PPU.Regs.PPUDATA

    | a == 0x2425 -> IgnoreFineScrollWrite -- ????

    -- .. more PPU regs
    -- PPU reg mirrors
    -- 0x4000 -- 0x401F -- APU and I/O regs

    -- sound is from 0x4000--0x4013, 0x4015, and 0x4017 (overlappiong Joy2)
    | a >= 0x4000 && a <= 0x4013 -> IgnoreSound

    | a == 0x4014 -> Dma
    | a == 0x4015 -> IgnoreSound
    | a == 0x4016 -> Joy1
    | a == 0x4017 -> Joy2

    --0x4020 .. 0x7FFF -- PRG Ram ??
    | a >= 0x8000 && a <= 0xBFFF -> Rom1 $ a `minusAddr` 0x8000
    | a >= 0xC000 && a <= 0xFFFF -> Rom2 $ a `minusAddr` 0xC000
    | otherwise ->
      error $ unwords ["CPU.Mem.decode ("<>tag<>") unsupported address:", show a]

data Decode
    = Ram Int
    | Rom1 Int
    | Rom2 Int
    | PPU PPU.Regs.Name
    | IgnoreFineScrollWrite
    | IgnoreSound
    | Dma
    | Joy1
    | Joy2
