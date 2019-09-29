
module PPU.Palette(
    Effect(..),
    State, state0,
    inter,
    ) where

import Control.Monad (ap,liftM)
import Data.Map.Strict as Map

import Byte

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Int -> Effect Byte
    Write :: Int -> Byte -> Effect ()

-- 32 bytes decoded as:
-- (25 bytes) -- uni bg col + 2 (spr/bg) x 4 x 3 cols
-- & 7 wasted bytes

data State = PAL (Map Int Byte) deriving (Show)

state0 :: State
state0 = PAL Map.empty

inter :: State -> Effect a -> (State, a)
inter s@(PAL m) = \case
    Ret x -> (s,x)
    Bind e f -> let (s',v) = inter s e in inter s' (f v)
    Write a b -> (PAL $ Map.insert a b m, ())
    Read a -> (s, Map.findWithDefault 0 a m)
