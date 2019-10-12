
module Honesty.Six502.Mem(
    Effect(..), reads,
    inter,
    ) where

import Data.Bits(testBit)
import Prelude hiding(reads)
import Control.Monad (ap,liftM)

import Honesty.Addr
import Honesty.Byte
import qualified Honesty.Log as Log
import qualified Honesty.Controller as Controller
import qualified Honesty.PPU.Regs as Regs
import qualified Honesty.PRG as PRG
import qualified Honesty.Ram2k as Ram2k
import qualified Honesty.Six502.MM as MM

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Addr -> Effect Byte
    Write :: Addr -> Byte -> Effect ()

reads :: Addr -> Effect [Byte]
reads addr = do
    byte0 <- Read addr
    byte1 <- Read $ addr `addAddr` 1
    byte2 <- Read $ addr `addAddr` 2
    return [byte0,byte1,byte2]

inter :: (Maybe PRG.ROM,PRG.ROM) -> Effect a -> MM.Effect a
inter (optPrg1,prg2) = loop  where
  loop :: Effect a -> MM.Effect a
  loop = \case
    Ret x -> return x
    Bind e f -> do v <- loop e; loop (f v)

    Read addr -> case decode "read" addr of
        Ram x -> MM.Ram (Ram2k.Read x)
        Rom1 x -> return $ PRG.read prg1 x
        Rom2 x -> return $ PRG.read prg2 x
        PPU reg -> MM.Reg (Regs.Read reg)
        IgnoreSound -> error $ "CPU.Mem, suprising read from sound reg"
        Joy1 -> MM.Con $ Controller.Read
        Joy2 -> do
            let b :: Byte = 0 -- no joystick 2
            return b
        Dma -> error $ "CPU.Mem, suprising read from DMA"
        NoIdea -> do
            MM.Log $ Log.message $ "CPU.Mem, suprising read from NoIdea: " <> show addr
            return 0

    Write addr v -> case decode "write" addr of
        Ram x -> MM.Ram (Ram2k.Write x v)
        Rom1 _ -> error $ "CPU.Mem, illegal write to Rom bank 1 : " <> show addr
        Rom2 _ -> error $ "CPU.Mem, illegal write to Rom bank 2 : " <> show addr
        PPU reg -> MM.Reg (Regs.Write reg v)
        IgnoreSound -> return ()
        Joy1 -> do
            let bool = testBit v 0
            MM.Con $ Controller.Strobe bool
        Joy2 -> do
            --error $ "CPU.Mem, suprising write to Joy2 : " <> show addr
            return ()
        Dma -> loop (dma v)
        NoIdea ->
            error $ "CPU.Mem, suprising write to NoIdea: " <> show addr

    where prg1 = case optPrg1 of
              Just prg -> prg
              Nothing ->
                  --error "CPU.Mem, no prg in bank 1"
                  prg2 -- HACK, for Ice

dma :: Byte -> Effect ()
dma b = do
    flip mapM_ [0..255] $ \i -> do
        let a = addrOfHiLo b i
        v <- Read a
        Write 0x2004 v --OAMDATA

decode :: String -> Addr -> Decode
decode _tag a = if
    | a < 0x800 -> Ram $ fromIntegral $ unAddr a

    -- 3 mirrors -- wait to see if they are used...
    | a < 0x1000 -> Ram $ a `minusAddr` 0x800

    | a == 0x2000 -> PPU Regs.PPUCTRL
    | a == 0x2001 -> PPU Regs.PPUMASK
    | a == 0x2002 -> PPU Regs.PPUSTATUS
    | a == 0x2003 -> PPU Regs.OAMADDR
    | a == 0x2004 -> PPU Regs.OAMDATA
    | a == 0x2005 -> PPU Regs.PPUSCROLL
    | a == 0x2006 -> PPU Regs.PPUADDR
    | a == 0x2007 -> PPU Regs.PPUDATA

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

    | otherwise -> NoIdea

data Decode
    = Ram Int
    | Rom1 Int
    | Rom2 Int
    | PPU Regs.Name
    | IgnoreSound
    | Joy1
    | Joy2
    | Dma
    | NoIdea
