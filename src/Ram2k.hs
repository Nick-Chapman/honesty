
module Ram2k(
    Effect(..),
    MState, newMState, interIO,
    ) where

import Prelude hiding (init,read)
import Control.Monad (ap,liftM)

import Data.Array.IO

import Six502.Values

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Int -> Effect Byte
    Write :: Int -> Byte -> Effect ()

size :: Int
size = 0x800 --2k

data MState = MState { name :: String, arr :: IOArray Int Byte }

newMState :: String -> IO MState
newMState name = do
    arr <- newArray (0,size-1) 0
    return $ MState {name,arr}

interIO :: MState -> Effect a -> IO a
interIO m@MState{name,arr} = \case
    Ret x -> return x
    Bind e f -> do v <- interIO m e; interIO m (f v)
    Write a b -> do
        --print ("WRITE",name,a,b)
        bounds <- getBounds arr
        if inRange bounds a
            then writeArray arr a b
            else error $ "Ram2k(" <> name <> ").write: " <> show a
    Read a -> do
        bounds <- getBounds arr
        if inRange bounds a
            then do b <- readArray arr a
                    --print ("READ",name,a,b)
                    return b
            else error $ "Ram2k(" <> name <> ").read: " <> show a
