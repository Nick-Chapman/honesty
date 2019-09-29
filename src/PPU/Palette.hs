
module PPU.Palette(
    Effect(..),
    State, state0,
    inter,
    ) where

import Six502.Values

import Control.Monad (ap,liftM)
instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Int -> Effect Byte
    Write :: Int -> Byte -> Effect ()

data State -- 1 uni bg col + 2 (spr/bg) x 4 x 3 cols

state0 :: State
state0 = undefined

inter :: State -> Effect a -> (State, a)
inter s = \case
    Ret x -> (s,x)
    Bind e f -> let (s',v) = inter s e in inter s' (f v)
    Write _a _b -> (s,()) -- error $ "Palette.write" -- TODO: do something!
    Read a -> error $ "Palette.read:" <> show a
