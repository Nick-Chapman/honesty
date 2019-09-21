
module Ram2k(
    Effect(..),
    State, init, run,
    ) where

import Prelude hiding (init,read)
import Control.Monad (ap,liftM)
import Data.Map(Map)
import qualified Data.Map as Map

import Six502.Values

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Int -> Effect Byte
    Write :: Int -> Byte -> Effect ()

data State = State { name :: String, ram :: Map Int Byte }
    deriving Show

size :: Int
size = 0x800 --2k

init :: String -> State
init name = State { name, ram = Map.empty }

run :: State -> Effect a -> (State, a)
run state = \case
    Ret x -> (state,x)
    Bind e f -> let (state',a) = run state e in run state' (f a)
    Read a -> (state, read state a)
    Write a b -> (write state a b, ())

read :: State -> Int -> Byte
read State{name,ram} a = if
    | inRange a -> Map.findWithDefault 0x0 a ram -- or handle uninitialized specially?
    | otherwise -> error $ "Ram2k(" <> name <> ").read: " <> show a

write :: State -> Int -> Byte -> State
write state@State{name,ram} a b = if
    | inRange a -> state { ram = Map.insert a b ram }
    | otherwise -> error $ "Ram2k(" <> name <> ").write: " <> show a

inRange :: Int -> Bool
inRange a = a >= 0 && a < size
