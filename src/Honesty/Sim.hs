
module Honesty.Sim(
    Effect(..),
    trace,
    Frames(..),frames,
    ) where

import Control.Monad (ap,liftM,when)
import Data.Set as Set

import Honesty.Nes as Nes
import Honesty.PPU.Render(Display)
import Honesty.Six502.Disassembler(ljust,displayOpLine)
import qualified Honesty.Controller as Controller
import qualified Honesty.Six502.Cpu as Six502.Cpu
import qualified Honesty.Six502.Emu as Six502.Emu
import qualified Honesty.Six502.Decode as Six502.Decode

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

trace :: Int -> Nes.RamRom -> Effect a -> IO ()
trace n rr eff = do _ <- loop n eff; return () where
    loop :: Int -> Effect a -> IO (Maybe (Int,a))
    loop n  = \case
        Ret a -> return $ Just (n,a)
        Bind e f -> loop n e >>= \case Nothing -> return Nothing; Just (n,v) -> loop n (f v)
        Render _ -> return $ Just (n,())
        Trace ns -> do printNS rr ns; return $ if n==1 then Nothing else Just (n-1,())
        SampleButtons -> return $ Just (n,Set.empty)
        IO io -> do v <- io; return $ Just (n,v)

newtype Frames a = Frames { unFrames :: Buttons -> IO (a,Frames a) }

frames :: Bool -> Nes.RamRom -> Buttons -> Effect () -> Frames Display
frames trace _rr = loop $ \_ ()-> error "run out of frames" where
    loop :: (Buttons -> a -> Frames Display) -> Buttons -> Effect a -> Frames Display
    loop k buttons = \case
        Ret a -> k buttons a
        Bind e f -> loop (\buttons v -> loop k buttons (f v)) buttons e
        Render display -> Frames $ \buttons -> return (display,k buttons ())
        Trace _ns -> do
            Frames $ \buttons -> do
                when trace $ printNS _rr _ns
                unFrames (k buttons ()) buttons

        SampleButtons -> k buttons buttons
        IO io -> Frames $ \buttons -> do v <- io; unFrames (k buttons v) buttons

printNS :: Nes.RamRom -> Nes.State -> IO ()
printNS rr ns@Nes.State{cpu,cc} = do
    let Six502.Cpu.State{Six502.Cpu.pc} = cpu
    bytes <- Six502.Emu.readFromAddr ns rr pc
    let op = Six502.Decode.decode1 bytes
    let col = 48
    let s = ljust col (displayOpLine pc op) <> show cpu  <> " " <> show cc
    putStrLn s
