
module Honesty.Emu(
    Effect, neverStopping,
    interpret,
    ) where

import Control.Monad(ap,liftM,when)
import Control.Monad(forever)
import Data.Set as Set

import Honesty.Nes as Nes
import Honesty.Six502.Cycles(Cycles)
import qualified Honesty.Controller as Controller
import qualified Honesty.NesRam as NesRam
import qualified Honesty.PPU.Regs as Regs
import qualified Honesty.PPU.Render as PPU
import qualified Honesty.Sim as Simulation
import qualified Honesty.Six502.Emu as Cpu
import qualified Honesty.Log as Log

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

type Buttons = Set Controller.Button

neverStopping :: Effect ()
neverStopping = forever stepOneFrame
    where
        stepOneFrame = do
            e <- IsNmiEnabled
            when e $ do
                Log $ Log.message "---[NMI]-----------------------------------------------"
                TriggerNMI
            runCpu (cyclesInVBlank)
            SetVBlank False
            Log $ Log.message "===[render]==============================================="
            Render
            runCpu (cyclesPerFrame - cyclesInVBlank)
            SetVBlank True

        cyclesPerFrame :: Cycles
        cyclesPerFrame = 29780

        cyclesInVBlank :: Cycles
        cyclesInVBlank = fromIntegral ((8200::Int) `div` 3)

        runCpu :: Cycles -> Effect ()
        runCpu n = if n < 0 then return () else do
            buttons <- Buttons
            cc <- RunCpuInstruction buttons
            runCpu (n - cc)

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    SetVBlank :: Bool -> Effect ()
    Render :: Effect ()
    Buttons :: Effect Buttons
    RunCpuInstruction :: Buttons -> Effect Cycles
    IsNmiEnabled :: Effect Bool
    TriggerNMI :: Effect ()
    Log :: Log.Effect a -> Effect a

interpret :: Bool -> Nes.RamRom -> Nes.State -> Effect () -> Simulation.Effect ()
interpret debug rr@Nes.RamRom{ram} state step = do (_state,()) <- loop state step; return () where
    loop :: Nes.State -> Effect a -> Simulation.Effect (Nes.State, a)
    loop s@Nes.State{fn,cc,regs,pal,oam} = \case
        Ret x -> return (s,x)
        Bind e f -> do (s,v) <- loop s e;  loop s (f v)
        SetVBlank bool ->
            return (s {regs = Regs.setVBlank regs bool}, ())
        Render -> do
            display <- Simulation.IO $ NesRam.inter debug fn cc ram $ NesRam.InVram (PPU.render fn rr regs pal oam)
            Simulation.Render display
            return (s {fn = fn + 1},())
        Buttons -> do
            buttons <- Simulation.SampleButtons
            return (s,buttons)
        RunCpuInstruction buttons -> do
            Simulation.Trace s
            Simulation.IO $ NesRam.inter debug fn cc ram (Cpu.cpuInstruction debug fn rr buttons s)
        IsNmiEnabled -> do
            let e = Regs.isEnabledNMI regs
            return (s,e)
        TriggerNMI -> do
            Simulation.Trace s
            s <- Simulation.IO $ NesRam.inter debug fn cc ram $ Cpu.triggerNMI debug fn rr s
            return (s,())
        Log e -> do
            v <- Simulation.IO $ Log.interIO debug fn cc e
            return (s,v)
