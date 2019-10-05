
module PPU.PMem( -- address space mapping for the PPU
    Effect(..),
    inter
    ) where

import Control.Monad (ap,liftM)

import Addr
import Byte
import CHR
import qualified Ram2k
import qualified PPU.Palette as Palette
import qualified PPU.PRam as PRam

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Addr -> Effect Byte
    Write :: Addr -> Byte -> Effect ()

inter :: CHR.ROM -> Effect a -> PRam.Effect a
inter chr = \case
    Ret x -> return x
    Bind e f -> do v <- inter chr e; inter chr (f v)

    Read addr -> case decode addr of
        Ram a -> PRam.InVRam (Ram2k.Read a)
        Rom a -> return $ CHR.read chr a
        PaletteRam _ -> PRam.Error $ "Suprising read from PaletteRam:" <> show addr
        --PaletteRam a -> PRam.InPalette (Palette.Read a) -- see if needed
        Error s -> PRam.Error $ "(read)" <> s

    Write addr b -> case decode addr of
        Ram a -> PRam.InVRam (Ram2k.Write a b)
        Rom _ -> PRam.Error $ "Illegal write to Rom:" <> show addr
        PaletteRam a -> PRam.InPalette (Palette.Write a b)
        Error s -> PRam.Error $ "(write)" <> s

decode :: Addr -> Decode
decode a = if
    | a < 0x2000 -> Rom $ fromIntegral $ unAddr a
    | a < 0x2800 ->  Ram $ a `minusAddr` 0x2000
    | a < 0x3000 ->  Ram $ a `minusAddr` 0x2800

--    | a >= 0x3F00 && a < 0x3F1F -> PaletteRam $ a `minusAddr` 0x3F00 -- BUG 0x3F1F

    -- palette mirrors, thought these might fix DK, but no :(
    | a == 0x3F10 -> PaletteRam 0
    | a == 0x3F14 -> PaletteRam 4
    | a == 0x3F18 -> PaletteRam 8
    | a == 0x3F1C -> PaletteRam 12

    | a >= 0x3F00 && a <= 0x3F1F -> PaletteRam $ a `minusAddr` 0x3F00

    -- mirrors... wait and see if they are used
--    | a < 0x3800 ->  a `minusAddr` 0x3000
--    | a < 0x4000 ->  a `minusAddr` 0x3800
    | otherwise ->  Error $ "PPU.Mem.decode, unknown address: " <> show a

data Decode
    = Ram Int
    | Rom Int
    | PaletteRam Int
    | Error String
