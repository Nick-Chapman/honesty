
module Honesty.Log(
    Effect(..),
    message,
    interIO
    ) where

import Control.Monad (ap,liftM,when)

import Honesty.Six502.Cycles

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

message :: String -> Effect ()
message = Log

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Log :: String -> Effect ()
    IO :: IO a -> Effect a

interIO :: Bool -> Int -> Cycles -> Effect a -> IO a
interIO debug fn cc = loop where
    loop :: Effect a -> IO a
    loop = \case
        Ret x -> return x
        Bind e f -> do v <- loop e; loop (f v)
        Log s -> when debug $ putStrLn $ "frame = " <> show fn <> ", " <> show cc <> " : " <> s
        IO io -> io
