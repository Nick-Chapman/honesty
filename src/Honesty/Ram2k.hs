
module Honesty.Ram2k(
    Effect(..),
    MState, newMState, interIO,
    ) where

import Control.Monad (ap,liftM,when)
import Data.Array.IO
import Prelude hiding (init,read)

import Honesty.Byte
import Honesty.Six502.Cycles
import qualified Honesty.Log as Log

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Int -> Effect Byte
    Write :: Int -> Byte -> Effect ()
    IO :: IO a -> Effect a
    Log :: Log.Effect a -> Effect a

size :: Int
size = 0x800 --2k

data MState = MState { trace :: Bool, name :: String, arr :: IOArray Int Byte }

newMState :: Bool -> String -> IO MState
newMState trace name = do
    arr <- newArray (0,size-1) 0
    return $ MState {trace, name,arr}

interIO :: Bool -> Int -> Cycles -> MState -> Effect a -> IO a
interIO debug fn cc MState{trace,name,arr} = Log.interIO debug fn cc . loop where
  loop :: Effect a -> Log.Effect a
  loop = \case
    Ret x -> return x
    Bind e f -> do v <- loop e; loop (f v)
    Write a b -> do
        when trace $ Log.message $ "Ram2k(" <> name <> ").write: " <> show a
        Log.IO $ do
            bounds <- getBounds arr
            if inRange bounds a
                then writeArray arr a b
                else error $ "Ram2k(" <> name <> ").write: " <> show a
    Read a -> Log.IO $ do
        bounds <- getBounds arr
        if inRange bounds a
            then readArray arr a
            else error $ "Ram2k(" <> name <> ").read: " <> show a

    IO io -> Log.IO io
    Log eff -> eff
