
module Honesty.PPU.PRam(
    Effect(..),
    inter,
    ) where

import Control.Monad (ap,liftM)

import Honesty.Six502.Cycles
import qualified Honesty.PPU.OAM as OAM
import qualified Honesty.PPU.Palette as Palette
import qualified Honesty.Ram2k as Ram2k

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    InVRam :: Ram2k.Effect a -> Effect a
    InPalette :: Palette.Effect a -> Effect a
    InOAM :: OAM.Effect a -> Effect a
    Error :: String -> Effect a

type State = (Palette.State, OAM.State)

inter :: Cycles -> State -> Effect a -> Ram2k.Effect (State, a)
inter cc s@(pal,oam) = \case
    Ret x -> return (s,x)
    Bind e f -> do (s,v) <- inter cc s e; inter cc s (f v)
    InVRam eff -> do v <- eff; return (s,v)
    InPalette eff -> let (pal',v) = Palette.inter pal eff in return ((pal',oam),v)
    InOAM eff -> let (oam',v) = OAM.inter oam eff in return ((pal,oam'),v)
    Error mes  -> error $ show cc <> ":" <> mes
