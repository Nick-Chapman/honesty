
module Log(
    Effect(..),
    message,
    interIO
    ) where

import Control.Monad (ap,liftM)
import Six502.Cycles

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

message :: Show a => a -> Effect ()
message a = Log (show a)

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Log :: String -> Effect ()

interIO :: Cycles -> Effect a -> IO a
interIO cc = loop where
    loop :: Effect a -> IO a
    loop = \case
        Ret x -> return x
        Bind e f -> do v <- loop e; loop (f v)
        Log s -> putStrLn $ show cc <> ":" <> s
