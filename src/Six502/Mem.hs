
module Six502.Mem( -- address space mapping for the CPU
    Effect(..), reads,
    rom1, rom2, RO,
    run,
    ) where

import Data.Bits(testBit)
import Prelude hiding(reads)
import Control.Monad (ap,liftM)

import Six502.Values
import qualified Ram2k
import qualified PPU.Regs
import qualified PRG
import qualified Controller as Con

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

data RO = RO { optPrg1 :: Maybe PRG.ROM, prg2 :: PRG.ROM }

rom1 :: PRG.ROM -> RO
rom1 prg2 = RO { optPrg1 = Nothing, prg2 }

rom2 :: PRG.ROM -> PRG.ROM -> RO
rom2 prg1 prg2 = RO { optPrg1 = Just prg1, prg2 }

type State = (Con.State,Ram2k.State)

run :: RO -> State -> Effect a -> PPU.Regs.Effect (State,a)
run ro@RO{optPrg1,prg2} state@(con,wram) = \case

    Ret x -> return (state,x)
    Bind e f -> do (state',v) <- run ro state e; run ro state' (f v)

    Read addr -> case decode "read" addr of
        Ram x -> do
            let (wram',v) = Ram2k.run wram (Ram2k.Read x)
            return ((con,wram'), v)

        Rom1 x -> return $ (state, PRG.read prg1 x)
        Rom2 x -> return $ (state, PRG.read prg2 x)
        PPU reg -> do v <- PPU.Regs.Read reg; return (state, v)
        IgnoreFineScrollWrite -> error $ "CPU.Mem, suprising read from fine-scroll reg"

        Dma -> error $ "CPU.Mem, Read DmaTODO"
        Joy1 -> do
            let (bool,con') = Con.read con
            let b :: Byte = if bool then 1 else 0
            return ((con',wram),b)
        Joy2 -> do
            let b :: Byte = 0 -- no joystick 2
            return (state,b)

    Write addr v -> case decode "write" addr of
        Ram x -> do
            let (wram',()) = Ram2k.run wram (Ram2k.Write x v)
            return $ ((con,wram'),())

        Rom1 _ -> error $ "CPU.Mem, illegal write to Rom bank 1 : " <> show addr
        Rom2 _ -> error $ "CPU.Mem, illegal write to Rom bank 2 : " <> show addr
        PPU reg -> do PPU.Regs.Write reg v; return (state, ())
        IgnoreFineScrollWrite -> return (state,())

        Dma -> return (state,()) -- TODO: support DMA !!!
        Joy1 -> do
            let bool = testBit v 0
            let con' = Con.strobe bool con
            return ((con',wram),())

        Joy2 -> do
            --error $ "CPU.Mem, suprising write to Joy2 : " <> show addr
            return (state,())

    where prg1 = case optPrg1 of Just prg -> prg; Nothing -> error "CPU.Mem, no prg in bank 1"

decode :: String -> Addr -> Decode
decode tag a = if
    | a < 0x800 -> Ram $ fromIntegral $ unAddr a

    -- 3 mirrors -- see if they are ever used...
    -- so far only touch the first two addresses in the first mirror, but never read/write them
    | a == 0x800 -> Ram undefined
    | a == 0x801 -> Ram undefined

    | a == 0x2000 -> PPU PPU.Regs.Control
    | a == 0x2001 -> PPU PPU.Regs.Mask
    | a == 0x2002 -> PPU PPU.Regs.Status
    | a == 0x2003 -> PPU PPU.Regs.OAMADDR
    | a == 0x2005 -> IgnoreFineScrollWrite
    | a == 0x2006 -> PPU PPU.Regs.PPUADDR
    | a == 0x2007 -> PPU PPU.Regs.PPUDATA

    -- .. more PPU regs
    -- PPU reg mirrors
    -- 0x4000 -- 0x401F -- APU and I/O regs

    | a == 0x4014 -> Dma
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
    | Dma
    | Joy1
    | Joy2
