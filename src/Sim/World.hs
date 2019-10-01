
module Sim.World(
    World(..), world0, updateWorld,
    printRun
    ) where

import Control.Monad (ap,liftM)
import Control.Monad (forever)
import Data.Tuple.Extra( (***) )
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad.State
import Text.Printf(printf)

import Addr
import Byte
import Nes
import NesFile
import PPU.Render(Display)
import Six502.Cycles
import Six502.Decode(decode1)
import Six502.Disassembler(ljust,displayOpLine)
import qualified CHR
import qualified Controller
import qualified NesRam
import qualified PPU.Graphics as Graphics(PAT,patFromBS)
import qualified PPU.Regs as Regs
import qualified PPU.Render as PPU
import qualified PRG
import qualified Six502.Cpu
import qualified Six502.Emu
import qualified Six502.MM as MM
import qualified Six502.Mem

type Buttons = Set Controller.Button

data World = World
    { frameCount :: Int
    , display :: Display
    , sys :: Sys
    , buttons :: Buttons
    , rr :: Nes.RamRom
    }

world0 :: String -> IO World
world0 path = do
    nesfile <- loadNesFile path
    let prg = prgOfNesFile nesfile
    let chr = chrOfNesFile nesfile
    let pc0 = resetAddr path prg
    ram <- NesRam.newMState
    let (pat1,pat2) = patPairFromBS (CHR.bytes chr)
    let rr = Nes.RamRom { ram, prg, chr, pat1, pat2 }
    let ns@Nes.State{regs,pal} = Nes.state0 pc0
    sys <- sysOfNesState rr ns
    display <- NesRam.inter ram $ NesRam.InVram (PPU.render rr regs pal)
    return $ World { frameCount = 0
                   , display
                   , sys
                   , buttons = Set.empty
                   , rr
                   }

patPairFromBS :: [Byte] -> (Graphics.PAT,Graphics.PAT)
patPairFromBS = (Graphics.patFromBS *** Graphics.patFromBS) . splitAt patSize
    where patSize = 0x1000 --2k (One PAT of 256 tiles)

resetAddr :: String -> PRG.ROM -> Addr
resetAddr path prg = do
    case path of
        "data/nestest.nes" -> 0xC000
        _ -> addrOfHiLo hi lo
            where
                lo = PRG.read prg 0x3ffc
                hi = PRG.read prg 0x3ffd

prgOfNesFile :: NesFile -> PRG.ROM
prgOfNesFile NesFile{prgs} =
    case prgs of
        [prg] -> prg
        --[_prg1,prg2] -> prg2
        _  ->
            error "emu, unexpected number of prg"

chrOfNesFile :: NesFile -> CHR.ROM
chrOfNesFile NesFile{chrs} =
    case chrs of
        [chr] -> chr
        _  ->
            error "emu, unexpected number of chr"

updateWorld :: Int -> Bool -> Bool -> Float -> World -> IO (Int,World)
updateWorld n trace _debug _delta world@World{frameCount,sys,buttons,rr} = loop n sys
    where
        loop :: Int -> Sys -> IO (Int,World)
        loop n = \case
            Sys_Render nesState display sysIO -> do
                let Nes.State{cc=_cc,pal=_pal} = nesState
                when _debug $ print (_showDelta _delta, frameCount,_cc)
                sys <- sysIO
                return (n, world { frameCount = frameCount + 1, display, sys })
            Sys_Trace nesState sysIO -> do
                when trace $ printNS rr nesState
                if n == 1 then return (0,world) else do
                    sys <- sysIO
                    loop (n-1) sys
            Sys_SampleButtons f -> do
                sys <- f buttons
                loop n sys

_showDelta :: Float -> String
_showDelta = printf "%.03g"

printRun :: Int -> World -> IO ()
printRun n w = do
    (n',w') <- updateWorld n True False 0 w
    if n'==0 then return () else
        printRun n' w'

printNS :: Nes.RamRom -> Nes.State -> IO ()
printNS rr ns@Nes.State{cpu,cc} = do
    let Six502.Cpu.State{Six502.Cpu.pc} = cpu
    bytes <- readFromAddr ns rr pc
    let op = Six502.Decode.decode1 bytes
    let col = 48
    let s = ljust col (displayOpLine pc op) <> show cpu  <> " " <> show cc
    putStrLn s

sysOfNesState :: Nes.RamRom -> Nes.State -> IO Sys
sysOfNesState rr nesState = do
    let finished (_::Nes.State) () = error "we finished"
    interpretStep rr nesState nesForever finished

cyclesPerFrame :: Cycles
cyclesPerFrame = 29780

cyclesInVBlank :: Cycles
cyclesInVBlank = fromIntegral ((8200::Int) `div` 3)

framesUntilPPuWarmsUp :: Int
framesUntilPPuWarmsUp = 0

nesForever :: Step ()
nesForever = do
    runCpu (cyclesPerFrame * fromIntegral framesUntilPPuWarmsUp)
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
    Buttons :: Step Buttons
    RunCpuInstruction :: Buttons -> Step Cycles
    IsNmiEnabled :: Step Bool
    TriggerNMI :: Step ()

instance Functor Step where fmap = liftM
instance Applicative Step where pure = return; (<*>) = ap
instance Monad Step where return = Step_Ret; (>>=) = Step_Bind

-- TODO: this continutaion based type is overly complex
data Sys
    = Sys_Render Nes.State Display (IO Sys)
    | Sys_Trace Nes.State (IO Sys)
    | Sys_SampleButtons (Buttons -> IO Sys)

interpretStep :: Nes.RamRom -> Nes.State -> Step a -> (Nes.State -> a -> IO Sys) -> IO Sys
interpretStep rr@Nes.RamRom{ram,prg} s@Nes.State{regs,pal} step k = case step of
    Step_Ret x -> k s x
    Step_Bind e f -> interpretStep rr s e $ \s v -> interpretStep rr s (f v) k
    SetVBlank bool -> do
        let s' = s {regs = Regs.setVBlank regs bool}
        k s' ()
    Render -> do
        display <- NesRam.inter ram $ NesRam.InVram (PPU.render rr regs pal)
        return $ Sys_Render s display $ k s ()
    Buttons ->
        return $ Sys_SampleButtons $ \buttons -> k s buttons
    RunCpuInstruction buttons -> do
        return $ Sys_Trace s $ do
            (s',cycles) <- NesRam.inter ram (cpuInstruction rr prg buttons s)
            k s' cycles
    IsNmiEnabled -> do
        let e = Regs.isEnabledNMI regs
        k s e
    TriggerNMI -> do
        return $ Sys_Trace s $ do
            s' <- NesRam.inter ram $ triggerNMI rr prg s
            k s' ()

-- TODO: move this code into Six502.Emu. Factor common code from the 3 defs

cpuInstruction :: Nes.RamRom -> PRG.ROM -> Buttons -> Nes.State -> NesRam.Effect (Nes.State,Cycles)
cpuInstruction Nes.RamRom{chr} prg2 buttons ns@Nes.State{cpu,cc} = do
    let opPrg1 = Nothing-- TODO
    let mm_eff = Six502.Mem.inter (opPrg1,prg2) (Six502.Emu.stepInstruction cpu)
    do
        ((cpu',cycles),ns') <- runStateT (MM.inter cc chr buttons mm_eff) ns
        let ns'' = ns' { cpu = cpu', cc = cc+cycles }
        return (ns'',cycles)

triggerNMI :: Nes.RamRom -> PRG.ROM -> Nes.State -> NesRam.Effect Nes.State
triggerNMI Nes.RamRom{chr} prg2 ns@Nes.State{cpu,cc} = do
    let opPrg1 = Nothing-- TODO
    let mm_eff = Six502.Mem.inter (opPrg1,prg2) (Six502.Emu.triggerNMI cpu)
    let buttons = Set.empty -- ok?
    do
        (cpu',ns') <- runStateT (MM.inter cc chr buttons mm_eff) ns
        return ns' { cpu = cpu' }

readFromAddr :: Nes.State -> Nes.RamRom -> Addr -> IO [Byte]
readFromAddr ns@Nes.State{cc} Nes.RamRom{prg,chr,ram} pc = do
    let opPrg1 = Nothing-- TODO
    let mem_eff = Six502.Mem.reads pc
    let _tag = "readFromAddr:"<>show ns
    let mm_eff = Six502.Mem.inter  (opPrg1,prg) mem_eff
    let buttons = Set.empty
    (bytes,_) <- NesRam.inter ram $ runStateT (MM.inter cc chr buttons mm_eff) ns
    return bytes
