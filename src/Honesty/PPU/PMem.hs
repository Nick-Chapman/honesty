
module Honesty.PPU.PMem( -- address space mapping for the PPU
    Effect(..),
    inter
    ) where

import Control.Monad (ap,liftM,when)
import Data.Bits

import Honesty.Addr
import Honesty.Byte
import qualified Honesty.CHR as CHR
import qualified Honesty.PPU.OAM as OAM
import qualified Honesty.PPU.PRam as PRam
import qualified Honesty.PPU.Palette as Palette
import qualified Honesty.Ram2k as Ram2k

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Addr -> Effect Byte
    Write :: Addr -> Byte -> Effect ()
    WriteOam :: Byte -> Byte -> Effect ()
    IO :: IO a -> Effect a

inter :: Bool -> CHR.ROM -> Effect a -> PRam.Effect a
inter debug chr = loop where

  loop:: Effect a -> PRam.Effect a
  loop = \case
    Ret x -> return x
    Bind e f -> do v <- loop e; loop (f v)

    Read addr -> case decode addr of
        Ram a -> PRam.InVRam (Ram2k.Read a)
        Rom a -> return $ CHR.read chr a
        PaletteRam _ -> PRam.Error $ "Suprising read from PaletteRam:" <> show addr
        Error s -> PRam.Error $ "(read)" <> s

    Write addr b -> case decode addr of
        Ram a -> PRam.InVRam (Ram2k.Write a b)
        Rom _ -> do
            when debug $ PRam.IO $ putStrLn $ "Suprising write to PPU Rom:" <> show addr
            return ()

        PaletteRam a -> PRam.InPalette (Palette.Write a b)
        Error s -> PRam.Error $ "(write)" <> s

    WriteOam a b ->
        PRam.InOAM (OAM.Write a b)

    IO e -> PRam.IO e


decode :: Addr -> Decode
decode aa = if
    -- TOOD: take account of horizontal/vertical NT mirroring as selected by the game cart
    | a < 0x2000 -> Rom $ fromIntegral $ unAddr a
    | a < 0x2800 ->  Ram $ a `minusAddr` 0x2000
    | a < 0x3000 ->  Ram $ a `minusAddr` 0x2800

    -- palette mirrors
    | a == 0x3F10 -> PaletteRam 0
    | a == 0x3F14 -> PaletteRam 4
    | a == 0x3F18 -> PaletteRam 8
    | a == 0x3F1C -> PaletteRam 12

    | a >= 0x3F00 && a <= 0x3F1F -> PaletteRam $ a `minusAddr` 0x3F00

    -- mirrors
    | a < 0x3800 ->  Ram $ a `minusAddr` 0x3000
    | a < 0x4000 ->  Ram $ a `minusAddr` 0x3800
    | otherwise ->  Error $ "PPU.Mem.decode, unknown address: " <> show a

    where a = Addr (unAddr aa .&. 0x3FFF) -- TODO: high address mirroring. necessary/correct?


data Decode
    = Ram Int
    | Rom Int
    | PaletteRam Int
    | Error String
