
module Honesty.PPU.OAM(
    Effect(..),
    State,state0, contents,
    inter,
    ) where

import Data.Map(Map)
import qualified Data.Map.Strict as Map

import Honesty.Byte

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Write :: Byte -> Byte -> Effect ()

data State = OAM (Map Byte Byte) deriving (Show)

state0 :: State
state0 = OAM Map.empty

contents :: State -> [Byte]
contents (OAM m) =
    map (\a -> Map.findWithDefault 0 a m) [0..255]

inter :: State -> Effect a -> (State,a)
inter s@(OAM m) = \case
    Ret x -> (s,x)
    Bind e f -> let (s',v) = inter s e in inter s' (f v)
    Write a b -> (OAM $ Map.insert a b m, ())
