
module Six502.Mem( -- address space mapping for the CPU
    Effect(..), reads,
    rom1, rom2, RO,
    run,
    ) where

import Prelude hiding(reads)
import Control.Monad (ap,liftM)

import Six502.Values
import qualified Ram2k
import qualified PPU.Regs
import qualified PRG

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

run :: RO -> Ram2k.State -> Effect a -> PPU.Regs.Effect (Ram2k.State,a)
run ro@RO{optPrg1,prg2} wram = \case

    Ret x -> return (wram,x)
    Bind e f -> do (wram',v) <- run ro wram e; run ro wram' (f v)

    Read addr -> case decode addr of
        Ram x -> return $ Ram2k.run wram (Ram2k.Read x)
        Rom1 x -> return $ (wram, PRG.read prg1 x)
        Rom2 x -> return $ (wram, PRG.read prg2 x)
        PPU reg -> do v <- PPU.Regs.Read reg; return (wram, v)

    Write addr v -> case decode addr of
        Ram x -> return $ Ram2k.run wram (Ram2k.Write x v)
        Rom1 _ -> error $ "CPU.Mem, illegal write to Rom bank 1 : " <> show addr
        Rom2 _ -> error $ "CPU.Mem, illegal write to Rom bank 2 : " <> show addr
        PPU reg -> do PPU.Regs.Write reg v; return (wram, ())

    where prg1 = case optPrg1 of Just prg -> prg; Nothing -> error "CPU.Mem, no prg in bank 1"

decode :: Addr -> Decode
decode a = if
    | a < 0x800 -> Ram $ fromIntegral $ unAddr a

    -- 3 mirrors -- see if they are ever used...
    -- so far only touch the first two addresses in the first mirror, but never read/write them
    | a == 0x800 -> Ram undefined
    | a == 0x801 -> Ram undefined

    | a == 0x2000 -> PPU PPU.Regs.Control
    | a == 0x2001 -> PPU PPU.Regs.Mask
    | a == 0x2002 -> PPU PPU.Regs.Status
    -- .. more PPU regs
    -- PPU reg mirrors
    -- 0x4000 -- 0x401F -- APU and I/O regs
     --0x4020 .. 0x7FFF -- PRG Ram ??
    | a >= 0x8000 && a <= 0xBFFF -> Rom1 $ a `minusAddr` 0x8000
    | a >= 0xC000 && a <= 0xFFFF -> Rom2 $ a `minusAddr` 0xC000
    | otherwise ->
      error $ "CPU.Mem.decode: unsupported address:" <> show a

data Decode
    = Ram Int
    | Rom1 Int
    | Rom2 Int
    | PPU PPU.Regs.Name
