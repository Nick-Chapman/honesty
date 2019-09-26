
-- try to wire everything up form the top

module Top(model0,
           pictureModel,
           handleEventModel,
           updateModel,
           ) where

import Control.Monad (ap,liftM)
import Control.Monad (forever)
import Data.Set(Set)
import Control.Monad.State
import Graphics.Gloss.Interface.IO.Game as Gloss(Event,Picture,pictures)

import Six502.Values
import qualified Controller
import qualified Ram2k
import qualified PPU.Regs
import qualified Six502.Emu
import qualified Six502.Mem
import qualified Graphics
import qualified PRG

data Model = Model
    { picture :: Gloss.Picture
    , sys :: Sys
    , buttons :: ButtonState
    }

pictureModel :: Model -> Gloss.Picture
pictureModel Model{picture} = picture

handleEventModel :: Gloss.Event -> Model -> IO Model
handleEventModel event model@Model{buttons} = do
    return $ model { buttons = updateButtons event buttons }

updateModel :: Float -> Model -> IO Model
updateModel _ model@Model{sys,buttons} = loop sys
    where
        loop :: Sys -> IO Model
        loop = \case
            Sys_Render screen sysIO -> do
                sys <- sysIO
                return $ model { picture = Graphics.pictureScreen screen , sys }
            Sys_Log nesState sysIO -> do
                print nesState
                sys <- sysIO
                loop sys
            Sys_SampleButtons f -> do
                sys <- f buttons
                loop sys


model0 :: Addr -> IO Model
model0 codeStartAddr = do
    rr <- ramRom0
    let s = nesState0 codeStartAddr
    sys <- sysOfNesState rr s
    return $ Model { picture = Gloss.pictures []
                   , sys
                   , buttons = buttons0
                   }


data NesRamRom = NesRamRom
    { vram :: Ram2k.MState,
      wram :: Ram2k.MState,
      prg :: PRG.ROM
    }

ramRom0 :: IO NesRamRom
ramRom0 = do
    let bytes = undefined -- program bytes
    vram <- Ram2k.newMState
    wram <- Ram2k.newMState
    let prg = PRG.init bytes
    return $ NesRamRom { vram, wram, prg }


data NesState = NesState
    { cpu :: CpuState
    , con :: ControllerState
    , regs :: RegsState
    }
    deriving (Show)


sysOfNesState :: NesRamRom -> NesState -> IO Sys
sysOfNesState rr nesState = do
    let finished (_::NesState) () = error "we finished"
    interpretStep rr nesState nesForever finished


nesState0 :: Addr -> NesState
nesState0 codeStartAddr =
    NesState { cpu = cpuState0 codeStartAddr
             , con = controllerState0
             , regs = regsState0
             }

newtype Cycles = Cycles { unCycles :: Int } deriving (Eq,Num,Ord)

cyclesPerFrame :: Cycles
cyclesPerFrame = 300_000 -- TODO: get real!

cyclesInVBlank :: Cycles
cyclesInVBlank = 1500 -- TODO: get real

framesUntilPPuWarmsUp :: Int
framesUntilPPuWarmsUp = 5 -- TODO: get real!

nesForever :: Step ()
nesForever = do
    runCpu (Cycles $ unCycles cyclesPerFrame * framesUntilPPuWarmsUp)
    forever nesOneFrame

nesOneFrame :: Step ()
nesOneFrame = do
    SetVBlank False
    Render
    runCpu (cyclesPerFrame - cyclesInVBlank)
    SetVBlank True
    e <- IsNmiEnabled
    if e then TriggerNMI else return ()
    runCpu (cyclesInVBlank)

runCpu :: Cycles -> Step ()
runCpu n = if n < 0 then return () else do
    buttons <- Buttons
    cc <- RunCpuInstruction buttons
    runCpu (n - cc)

data Step a where
    Step_Ret :: a -> Step a
    Step_Bind :: Step a -> (a -> Step b) -> Step b
    SetVBlank :: Bool -> Step ()
    Render :: Step ()
    Buttons :: Step ButtonState
    RunCpuInstruction :: ButtonState -> Step Cycles
    IsNmiEnabled :: Step Bool
    TriggerNMI :: Step ()

instance Functor Step where fmap = liftM
instance Applicative Step where pure = return; (<*>) = ap
instance Monad Step where return = Step_Ret; (>>=) = Step_Bind

data Logged = Log_NesState NesState | Log_NMI deriving (Show)

data Sys
    = Sys_Render Graphics.Screen (IO Sys) -- comes every instruction, every 2-7 or so cpu cycles
    | Sys_Log Logged (IO Sys) -- comes every 30k ish cpu cycles
    | Sys_SampleButtons (ButtonState -> IO Sys)

interpretStep :: NesRamRom -> NesState -> Step a -> (NesState -> a -> IO Sys) -> IO Sys
interpretStep rr@NesRamRom{vram,wram,prg} s@NesState{regs} step k = case step of
    Step_Ret x -> k s x
    Step_Bind e f -> interpretStep rr s e $ \s v -> interpretStep rr s (f v) k
    SetVBlank bool -> do
        let s' = s {regs = setVBlank regs bool}
        k s' ()
    Render -> do
        screen <- Ram2k.interIO vram (render regs)
        return $ Sys_Render screen $ k s ()
    Buttons ->
        return $ Sys_SampleButtons $ \buttons -> k s buttons
    RunCpuInstruction buttons -> do
        return $ Sys_Log (Log_NesState s) $ do
            (s',cycles) <- Ram2k.interIO wram (cpuInstruction prg buttons s)
            k s' cycles
    IsNmiEnabled -> do
        let e = isEnabledNMI regs
        k s e
    TriggerNMI -> do
        return $ Sys_Log (Log_NesState s) $ return $ Sys_Log Log_NMI $ do
            s' <- Ram2k.interIO wram (triggerNMI prg s)
            k s' ()

----------------------------------------------------------------------
-- more complex combinations

render :: RegsState -> Ram2k.Effect Graphics.Screen
render = undefined ppuRender

ppuRender :: RegsState -> PpuMemEff Graphics.Screen
ppuRender = undefined

cpuInstruction :: PRG.ROM -> ButtonState -> NesState -> Ram2k.Effect (NesState,Cycles)
cpuInstruction = undefined stepInstruction iCpuMem iCpuMemMapEff

triggerNMI :: PRG.ROM -> NesState -> Ram2k.Effect NesState
triggerNMI = undefined

----------------------------------------------------------------------
-- Cpu

type CpuState = Six502.Emu.Cpu

cpuState0 :: Addr -> CpuState
cpuState0 = Six502.Emu.cpu0

stepInstruction :: CpuState -> CpuMemEff (CpuState,Cycles)
stepInstruction = undefined

----------------------------------------------------------------------
-- CpuMemEff

type CpuMemEff a = Six502.Mem.Effect a

iCpuMem :: PRG.ROM -> CpuMemEff a -> CpuMemMapEff a
iCpuMem = undefined MM_Con MM_Reg MM_Ram

data CpuMemMapEff a
    = MM_Con (ControllerEff a)
    | MM_Reg (RegsEff a)
    | MM_Ram (Ram2k.Effect a)

iCpuMemMapEff :: ButtonState -> CpuMemMapEff a -> StateT (ControllerState,RegsState) Ram2k.Effect a
iCpuMemMapEff = undefined iController iRegs


----------------------------------------------------------------------
-- Buttons -- TODO: move to controller

newtype ButtonState = ButtonState (Set Controller.Button) deriving (Show)

updateButtons :: Gloss.Event -> ButtonState -> ButtonState
updateButtons = undefined

buttons0 :: ButtonState
buttons0 = undefined

----------------------------------------------------------------------
-- Controller

type ControllerState = Controller.State

controllerState0 :: ControllerState
controllerState0 = Controller.init

data ControllerEff a -- Strobe | Read -- TODO, in Controller

iController :: ButtonState -> ControllerEff a -> State ControllerState a
iController = undefined

----------------------------------------------------------------------
-- Regs

type RegsState = PPU.Regs.State

regsState0 :: RegsState
regsState0 = PPU.Regs.init

setVBlank :: RegsState -> Bool -> RegsState
setVBlank = undefined

isEnabledNMI :: RegsState -> Bool
isEnabledNMI = undefined

type RegsEff a = PPU.Regs.Effect a

iRegs :: RegsEff a -> RegsState -> PpuMemEff (RegsState, a)
iRegs e s = PPU.Regs.run s e

type PpuMemEff = Ram2k.Effect -- TODO: do PPU address mapping better
