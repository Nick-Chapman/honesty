
module PPU.PRam(
    Effect(..),
    inter,
    ) where

import Control.Monad (ap,liftM)

import Six502.Cycles
import qualified Ram2k
import qualified PPU.Palette as Palette

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    InVRam :: Ram2k.Effect a -> Effect a
    InPalette :: Palette.Effect a -> Effect a
    Error :: String -> Effect a

inter :: Cycles -> Palette.State -> Effect a -> Ram2k.Effect (Palette.State, a)
inter cc s = \case
    Ret x -> return (s,x)
    Bind e f -> do (s,v) <- inter cc s e; inter cc s (f v)
    InVRam eff -> do v <- eff; return (s,v)
    InPalette eff -> return $ Palette.inter s eff
    Error mes  -> error $ show cc <> ":" <> mes
