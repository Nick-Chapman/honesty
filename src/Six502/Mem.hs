
module Six502.Mem( -- address space mapping for the CPU
    Effect(..), reads,
    --decode, Decode(..),
    inter,
    ) where

import Data.Bits(testBit)
import Prelude hiding(reads)
import Control.Monad (ap,liftM)

import Six502.Values
import qualified PPU.Regs as Regs
import qualified PRG
import qualified Six502.MM as MM
import qualified Ram2k
import qualified Controller

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Addr -> Effect Byte
    Write :: Addr -> Byte -> Effect ()

reads :: Addr -> Effect [Byte]
reads addr = do  -- dislike this multi Reads interface now. only use for max of 3
    byte0 <- Read addr
    byte1 <- Read $ addr `addAddr` 1
    byte2 <- Read $ addr `addAddr` 2
    return [byte0,byte1,byte2]




type CpuMemEff a = Six502.Mem.Effect a


inter :: (Maybe PRG.ROM,PRG.ROM) -> CpuMemEff a -> MM.Effect a
inter s@(optPrg1,prg2) = \case

    Ret x -> return x
    Bind e f -> do v <- inter s e; inter s (f v)

    Read addr -> case decode "read" addr of
        Ram x -> MM.Ram (Ram2k.Read x)
        Rom1 x -> return $ PRG.read prg1 x
        Rom2 x -> return $ PRG.read prg2 x
        PPU reg -> MM.Reg (Regs.Read reg)
        -- ???
        IgnoreFineScrollWrite -> error $ "CPU.Mem, suprising read from fine-scroll reg"
        IgnoreSound -> error $ "CPU.Mem, suprising read from sound reg"
        Dma -> error $ "CPU.Mem, Read DmaTODO"
        Joy1 -> MM.Con $ Controller.Read
        Joy2 -> do
            let b :: Byte = 0 -- no joystick 2
            return b

    Write addr v -> case decode "write" addr of
        Ram x -> MM.Ram (Ram2k.Write x v)
        Rom1 _ -> error $ "CPU.Mem, illegal write to Rom bank 1 : " <> show addr
        Rom2 _ -> error $ "CPU.Mem, illegal write to Rom bank 2 : " <> show addr
        PPU reg -> MM.Reg (Regs.Write reg v)
        IgnoreFineScrollWrite -> return ()
        IgnoreSound -> return ()
        Dma -> return () -- TODO: support DMA !!!
        Joy1 -> do
            let bool = testBit v 0
            MM.Con $ Controller.Strobe bool

        Joy2 -> do
            --error $ "CPU.Mem, suprising write to Joy2 : " <> show addr
            return ()

    where prg1 = case optPrg1 of Just prg -> prg; Nothing -> error "CPU.Mem, no prg in bank 1"


decode :: String -> Addr -> Decode
decode tag a = if
    | a < 0x800 -> Ram $ fromIntegral $ unAddr a

    -- 3 mirrors -- wait to see if they are used...
    -- | a < 0x1000 -> Ram $ a `minusAddr` 0x800

    | a == 0x2000 -> PPU Regs.Control
    | a == 0x2001 -> PPU Regs.Mask
    | a == 0x2002 -> PPU Regs.Status
    | a == 0x2003 -> PPU Regs.OAMADDR
    | a == 0x2005 -> IgnoreFineScrollWrite
    | a == 0x2006 -> PPU Regs.PPUADDR
    | a == 0x2007 -> PPU Regs.PPUDATA

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
    | PPU Regs.Name
    | IgnoreFineScrollWrite
    | IgnoreSound
    | Dma
    | Joy1
    | Joy2
