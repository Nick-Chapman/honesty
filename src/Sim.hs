
module Sim(
    Effect(..),
    trace,
    Frames(..),frames,
    ) where

import Control.Monad (ap,liftM)
import Data.Set as Set

import Nes
import PPU.Render(Display)
import qualified Controller

type Buttons = Set Controller.Button

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Render :: Display -> Effect ()
    Trace :: Nes.State -> Effect ()
    SampleButtons :: Effect Buttons
    IO :: IO a -> Effect a

trace :: Int -> (Nes.State -> IO ()) -> Effect a -> IO ()
trace n print eff = do _ <- loop n eff; return () where
    loop :: Int -> Effect a -> IO (Maybe (Int,a))
    loop n  = \case
        Ret a -> return $ Just (n,a)
        Bind e f -> loop n e >>= \case Nothing -> return Nothing; Just (n,v) -> loop n (f v)
        Render _ -> return $ Just (n,())
        Trace ns -> do print ns; return $ if n==1 then Nothing else Just (n-1,())
        SampleButtons -> return $ Just (n,Set.empty)
        IO io -> do v <- io; return $ Just (n,v)

newtype Frames a = Frames { unFrames :: Buttons -> IO (a,Frames a) }

frames :: Buttons -> Effect () -> Frames Display
frames = loop $ \_ ()-> error "run out of frames" where
    loop :: (Buttons -> a -> Frames Display) -> Buttons -> Effect a -> Frames Display
    loop k buttons = \case
        Ret a -> k buttons a
        Bind e f -> loop (\buttons v -> loop k buttons (f v)) buttons e
        Render display -> Frames $ \buttons -> return (display,k buttons ())
        Trace _ns -> k buttons ()
        SampleButtons -> k buttons buttons
        IO io -> Frames $ \buttons -> do v <- io; unFrames (k buttons v) buttons
