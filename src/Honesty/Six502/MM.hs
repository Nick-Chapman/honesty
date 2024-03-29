
module Honesty.Six502.MM( -- memory map
    Effect(..),
    inter
    ) where

import Control.Monad (ap,liftM)
import Control.Monad.State (StateT(..),lift)
import Data.Set (Set)
import Honesty.Nes as Nes (State(..))
import Honesty.Six502.Cycles (Cycles)
import qualified Honesty.CHR as CHR (ROM)
import qualified Honesty.Controller as Controller (Effect,Button,inter)
import qualified Honesty.Log as Log (Effect,interIO)
import qualified Honesty.NesRam as NesRam (Effect(..))
import qualified Honesty.PPU.PMem as PMem (NametableMirroring,inter)
import qualified Honesty.PPU.PRam as PRam (inter)
import qualified Honesty.PPU.Regs as Regs (Effect,inter)
import qualified Honesty.Ram2k as Ram2k (Effect)

type Buttons = Set Controller.Button

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Con :: Controller.Effect a -> Effect a
    Reg :: Regs.Effect a -> Effect a
    Ram :: Ram2k.Effect a -> Effect a
    Log :: Log.Effect a -> Effect a
    Delay :: Cycles -> Effect ()

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

inter :: Bool -> Int -> PMem.NametableMirroring -> CHR.ROM -> Buttons -> Effect a -> StateT Nes.State NesRam.Effect a
inter debug fn ntm chr buttons = loop where
  loop :: Effect a -> StateT Nes.State NesRam.Effect a
  loop = \case
    Ret x -> return x
    Bind e f -> do v <- loop e; loop (f v)

    Con eff -> do
        StateT $ \ns@Nes.State{cc,con} -> NesRam.EmbedIO $ do
            (v,con') <- Log.interIO debug fn cc $ Controller.inter buttons con eff
            return (v, ns { con = con' })

    Reg eff -> do
        StateT $ \ns@Nes.State{cc,regs,pal,oam} -> NesRam.InVram $ do
            -- TODO: these 3 PPU interpreters should be merged and moved in to PPU subdir
            ((pal',oam'),(regs',v)) <- PRam.inter cc (pal,oam) $ PMem.inter ntm chr $ Regs.inter regs eff
            return (v, ns { regs = regs', pal = pal', oam = oam' })

    Ram eff -> lift $ NesRam.InWram eff

    Log eff -> do
        StateT $ \ns@Nes.State{cc} -> NesRam.EmbedIO $ do
            v <- Log.interIO debug fn cc eff
            return (v, ns)

    Delay cycles -> do
        StateT $ \ns@Nes.State{cc} -> NesRam.EmbedIO $ do
            return ((), ns { cc = cc + cycles })
