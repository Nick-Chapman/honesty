
module Six502.MM( -- memory map
    Effect(..),
    inter
    ) where

import Control.Monad (ap,liftM)
import Control.Monad.State
import Data.Set(Set)

import Nes
import Six502.Cycles
import qualified CHR
import qualified Controller
import qualified Log
import qualified NesRam
import qualified PPU.PMem as PMem
import qualified PPU.PRam as PRam
import qualified PPU.Regs as Regs
import qualified Ram2k

type Buttons = Set Controller.Button

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Con :: Controller.Effect a -> Effect a
    Reg :: Regs.Effect a -> Effect a
    Ram :: Ram2k.Effect a -> Effect a

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

inter :: Cycles -> CHR.ROM -> Buttons -> Effect a -> StateT Nes.State NesRam.Effect a
inter cc chr buttons = \case
    Ret x -> return x
    Bind e f -> do v <- inter cc chr buttons e; inter cc chr buttons (f v)

    Con eff -> do
        StateT $ \ns@Nes.State{con} -> NesRam.EmbedIO $ do
            (v,con') <- Log.interIO cc $ Controller.inter buttons con eff
            return (v, ns { con = con' })

    Reg eff -> do
        StateT $ \ns@Nes.State{regs,pal} -> NesRam.InVram $ do
            -- TODO: these 3 PPU interpreters should be merged and moved in to PPU subdir
            (pal',(regs',v)) <- PRam.inter cc pal $ PMem.inter chr $ Regs.inter cc chr regs eff
            return (v, ns { regs = regs', pal = pal' })

    Ram eff -> lift $ NesRam.InWram eff

