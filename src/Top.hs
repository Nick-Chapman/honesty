
-- try to wire everything up form the top

module Top(gloss,
           printRun,
           model0,
           pictureModel,
           handleEventModel,
           updateModel,
           ) where

import Data.Bits(testBit)

import Control.Monad (ap,liftM)
import Control.Monad (forever)
import Data.Set(Set)
import Control.Monad.State

import Graphics.Gloss (translate,scale,pictures)
import Graphics.Gloss.Interface.IO.Game(Event(..),Key(..),SpecialKey(..),KeyState(..))

import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Six502.Values
import qualified Controller
import qualified Ram2k
import qualified NesRam
import qualified PPU.Regs
import qualified Six502.Emu
import qualified Graphics
import qualified PRG
import NesFile

import Graphics(CHR,pictureScreen,screenTiles,Screen)

import Six502.Emu (Cycles(..))
import Six502.Mem(Effect(..),Decode(..),decode,reads)

import Six502.Disassembler(ljust,displayOpLine)
import Six502.Decode(decode1)

printNS :: NesRamRom -> NesState -> IO ()
printNS rr ns@NesState{cpu,cc,regs=_} = do
    let Six502.Emu.Cpu{Six502.Emu.pc} = cpu
    --print pc
    bytes <- readFromAddr ns rr pc
    let op = decode1 bytes
    let col = 48
    let s = ljust col (displayOpLine pc op) <> show cpu  <> " " <> show cc -- <> " -" <> show regs
    putStrLn s

_printNSx :: NesRamRom -> NesState -> IO ()
_printNSx rr ns@NesState{cpu,cc,regs} = do
    let Six502.Emu.Cpu{Six502.Emu.pc} = cpu
    bytes <- readFromAddr ns rr pc
    let op = decode1 bytes
    let col = 48
    let s = ljust col (displayOpLine pc op) <> show cpu  <> " " <> show cc <> " -" <> show regs
    --let s = show pc <> " : " <> show cpu  <> " " <> show cc <> " -" <> show regs
    putStrLn s




gloss :: String -> Bool -> Int -> IO ()
gloss path fg sc = do
    model <- model0 path
    Gloss.playIO dis (Gloss.greyN 0.3) fps model
        (\  m -> return $ doPosition (pictureModel m))
        (\e m -> handleEventModel e m)
        (\d m -> updateModel d m)
    where
        dis = if fg
              then Gloss.FullScreen
              else Gloss.InWindow "NES" (sc * x,sc * y) (0,0)

        fps = 2 -- 60 -- slow for dev

        doPosition = doScale . doBorder . doTransOriginUL
        doScale = scale scF (-scF) -- The "-" flips the vertical
        scF = fromIntegral sc

        doBorder = translate 10 10
        doTransOriginUL = translate (- ((fromIntegral x)/2)) (- ((fromIntegral y)/2))

        x = 800
        y = 400


data Model = Model
    { picture :: Gloss.Picture
    , sys :: Sys
    , buttons :: ButtonState
    , rr :: NesRamRom
    }

pictureModel :: Model -> Gloss.Picture
pictureModel Model{picture} = picture

handleEventModel :: Gloss.Event -> Model -> IO Model
handleEventModel event model@Model{buttons} = do
    case event of
        EventKey (SpecialKey KeyEsc) Down _ _ -> error "quit"
        _ ->
            return $ model { buttons = updateButtons event buttons }

updateModel :: Float -> Model -> IO Model
updateModel _ model@Model{sys,buttons,rr} = loop sys
    where
        loop :: Sys -> IO Model
        loop = \case
            Sys_Render display sysIO -> do
                sys <- sysIO
                return $ model { picture = makePicture rr display , sys }
            Sys_Log (Log_NesState _nesState) sysIO -> do
                --printNSx rr nesState
                sys <- sysIO
                loop sys
            Sys_Log Log_NMI sysIO -> do
                print "--------------------NMI--------------------"
                sys <- sysIO
                loop sys
            Sys_Log (Log_Info s) sysIO -> do
                putStrLn $ "***INFO: " <> s
                sys <- sysIO
                loop sys
            Sys_SampleButtons f -> do
                sys <- f buttons
                loop sys


printRun :: Int -> Model -> IO ()
printRun n Model{buttons,sys,rr} = loop n sys
    where
        loop :: Int -> Sys -> IO ()
        loop n sys = case sys of
            Sys_Render _ sysIO -> do
                sys <- sysIO
                loop n sys
            Sys_Log (Log_NesState nesState) sysIO -> do
                printNS rr nesState
                if n == 1 then return () else do
                    sys <- sysIO
                    loop (n-1) sys
            Sys_Log Log_NMI sysIO -> do
                print "--------------------NMI--------------------"
                sys <- sysIO
                loop n sys
            Sys_Log (Log_Info s) sysIO -> do
                putStrLn $ "***INFO: " <> s
                sys <- sysIO
                loop n sys
            Sys_SampleButtons f -> do
                sys <- f buttons
                loop n sys



data Display = Display
    { bg1 :: Screen
    , bg2 :: Screen }

makePicture :: NesRamRom -> Display -> Gloss.Picture
makePicture NesRamRom{chr1,chr2} Display{bg1,bg2} = do
    let left = screenTiles chr1
    let right = screenTiles chr2
    pictures
        [ pictureScreen bg1
        , translate 300 0 $ pictureScreen bg2
        , translate 600 0 $ pictureScreen left
        , translate 600 150 $ pictureScreen right
        ]


model0 :: String -> IO Model
model0 path = do
    nesfile@NesFile{chrs=[(chr1,chr2)]} <- loadNesFile path
    let prg = prgOfNesFile nesfile
    let pc0 = resetAddr path prg
    ram <- NesRam.newMState
    let rr = NesRamRom { ram, prg, chr1, chr2 }
    let s = nesState0 pc0
    sys <- sysOfNesState rr s
    return $ Model { picture = Gloss.pictures []
                   , sys
                   , buttons = buttons0
                   , rr
                   }


resetAddr :: String -> PRG.ROM -> Addr
resetAddr path prg = do
    let bytes = PRG.unROM prg
    case path of
        "data/nestest.nes" -> 0xC000
        _ -> addrOfHiLo hi lo
            where
                lo = bytes !! 0x3ffc
                hi = bytes !! 0x3ffd

prgOfNesFile :: NesFile -> PRG.ROM
prgOfNesFile NesFile{prgs} =
    case prgs of
        [prg] -> prg
        --[_prg1,prg2] -> prg2
        _  ->
            error "emu, unexpected number of prg"

data NesRamRom = NesRamRom
    { ram :: NesRam.MState,
      prg :: PRG.ROM,
      chr1 :: CHR,
      chr2 :: CHR
    }


data NesState = NesState
    { cpu :: CpuState
    , con :: ControllerState
    , regs :: RegsState
    , cc :: Cycles
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
             , cc = Cycles 7 -- for nestest.nes
             }

cyclesPerFrame :: Cycles
cyclesPerFrame = 29780 -- Cycles $ 89342 `div` 3 -- 30_000 -- TODO: get real!

cyclesInVBlank :: Cycles
cyclesInVBlank = Cycles $ 8200 `div` 3 -- TODO: get real

framesUntilPPuWarmsUp :: Int
framesUntilPPuWarmsUp = 0 -- TODO: get real!

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
    Log $ "enabled=" <> show e
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
    Log :: String -> Step ()

instance Functor Step where fmap = liftM
instance Applicative Step where pure = return; (<*>) = ap
instance Monad Step where return = Step_Ret; (>>=) = Step_Bind

data Logged = Log_NesState NesState | Log_NMI | Log_Info String

data Sys
    = Sys_Render Display (IO Sys) -- comes every instruction, every 2-7 or so cpu cycles
    | Sys_Log Logged (IO Sys) -- comes every 30k ish cpu cycles
    | Sys_SampleButtons (ButtonState -> IO Sys)

interpretStep :: NesRamRom -> NesState -> Step a -> (NesState -> a -> IO Sys) -> IO Sys
interpretStep rr@NesRamRom{ram,prg} s@NesState{regs} step k = case step of
    Step_Ret x -> k s x
    Step_Bind e f -> interpretStep rr s e $ \s v -> interpretStep rr s (f v) k
    SetVBlank bool -> do
        let s' = s {regs = setVBlank regs bool}
        k s' ()
    Render -> do
        screen <- NesRam.inter ram $ NesRam.InVram (render rr regs)
        return $ Sys_Render screen $ k s ()
    Buttons ->
        return $ Sys_SampleButtons $ \buttons -> k s buttons
    RunCpuInstruction buttons -> do
        return $ Sys_Log (Log_NesState s) $ do
            (s',cycles) <- NesRam.inter ram (cpuInstruction prg buttons s)
            k s' cycles
    IsNmiEnabled -> do
        let e = isEnabledNMI regs
        k s e
    TriggerNMI -> do
        return $ Sys_Log (Log_NesState s) $ return $ Sys_Log Log_NMI $ do
            s' <- NesRam.inter ram $ triggerNMI prg s
            k s' ()
    Log info ->
        return $ Sys_Log (Log_Info info) $ do
            k s ()

----------------------------------------------------------------------
-- more complex combinations

render :: NesRamRom -> PPU.Regs.State -> Ram2k.Effect Display
render NesRamRom{chr1,chr2} _regs = do
    -- Depending on nametable mirroring (V/H) as selcte by PPU_Regs
    -- shoud read 1st or 2nd K of the vram.
    -- Probably better to go via the memory map!

    kilobyte1 <- mapM (\a -> Ram2k.Read a) [0..0x3ff]
    kilobyte2 <- mapM (\a -> Ram2k.Read a) [0x400..0x7ff]

    -- depending on some other flag in the PPU.Regs,
    -- should pick chr1 or chr2
    let chrPick = True
    let chr = if chrPick then chr2 else chr1
    let bg1 = Graphics.screenBG kilobyte1 chr
    let bg2 = Graphics.screenBG kilobyte2 chr
    let display = Display { bg1, bg2 }
    return $ display


cpuInstruction :: PRG.ROM -> ButtonState -> NesState -> NesRam.Effect (NesState,Cycles)
cpuInstruction prg2 buttons NesState{cpu,con,regs,cc} = do
    let opPrg1 = Nothing-- TODO
    let mm_eff = iCpuMem (opPrg1,prg2) (Six502.Emu.stepInstruction cpu)
    do
        ((cpu',cycles),(con',regs')) <- runStateT (iCpuMemMapEff buttons mm_eff) (con,regs)
        let ns = NesState cpu' con' regs' (cc+cycles)
        return (ns,cycles)

triggerNMI :: PRG.ROM -> NesState -> NesRam.Effect NesState
triggerNMI prg2 NesState{cpu,con,regs,cc} = do
    let opPrg1 = Nothing-- TODO
    let mm_eff = iCpuMem (opPrg1,prg2) (Six502.Emu.triggerNMI cpu)
    let buttons = undefined
    do
        (cpu',(con',regs')) <- runStateT (iCpuMemMapEff buttons mm_eff) (con,regs)
        let ns = NesState cpu' con' regs' cc
        return ns

readFromAddr :: NesState -> NesRamRom -> Addr -> IO [Byte]
readFromAddr ns NesRamRom{prg,ram} pc = do
    let opPrg1 = Nothing-- TODO
    let mem_eff = Six502.Mem.reads pc
    let _tag = "readFromAddr:"<>show ns
    let mm_eff = iCpuMem  (opPrg1,prg) mem_eff
    let buttons = buttons0
    let con = controllerState0
    let regs = regsState0
    (bytes,_) <- NesRam.inter ram $ runStateT (iCpuMemMapEff buttons mm_eff) (con,regs)
    return bytes


----------------------------------------------------------------------
-- Cpu

type CpuState = Six502.Emu.Cpu

cpuState0 :: Addr -> CpuState
cpuState0 = Six502.Emu.cpu0

--stepInstruction :: CpuState -> CpuMemEff (CpuState,Cycles)
--stepInstruction = Six502.Emu.stepInstruction

----------------------------------------------------------------------
-- CpuMemEff

type CpuMemEff a = Six502.Mem.Effect a

-- TODO: MOve this code back into Cpu/Mem.hs
iCpuMem :: (Maybe PRG.ROM,PRG.ROM) -> CpuMemEff a -> CpuMemMapEff a
iCpuMem s@(optPrg1,prg2) = \case

    Ret x -> return x
    Bind e f -> do v <- iCpuMem s e; iCpuMem s (f v)

    Read addr -> case decode "read" addr of
        Ram x -> MM_Ram (Ram2k.Read x)
        Rom1 x -> return $ PRG.read prg1 x
        Rom2 x -> return $ PRG.read prg2 x
        PPU reg -> MM_Reg (PPU.Regs.Read reg)
        -- ???
        IgnoreFineScrollWrite -> error $ "CPU.Mem, suprising read from fine-scroll reg"
        IgnoreSound -> error $ "CPU.Mem, suprising read from sound reg"
        Dma -> error $ "CPU.Mem, Read DmaTODO"
        Joy1 -> MM_Con $ ControllerEff_Read
        Joy2 -> do
            let b :: Byte = 0 -- no joystick 2
            return b

    Write addr v -> case decode "write" addr of
        Ram x -> MM_Ram (Ram2k.Write x v)
        Rom1 _ -> error $ "CPU.Mem, illegal write to Rom bank 1 : " <> show addr
        Rom2 _ -> error $ "CPU.Mem, illegal write to Rom bank 2 : " <> show addr
        PPU reg -> MM_Reg (PPU.Regs.Write reg v)
        IgnoreFineScrollWrite -> return ()
        IgnoreSound -> return ()
        Dma -> return () -- TODO: support DMA !!!
        Joy1 -> do
            let bool = testBit v 0
            MM_Con $ ControllerEff_Strobe bool

        Joy2 -> do
            --error $ "CPU.Mem, suprising write to Joy2 : " <> show addr
            return ()

    where prg1 = case optPrg1 of Just prg -> prg; Nothing -> error "CPU.Mem, no prg in bank 1"

----------------------------------------------------------------------
-- CpuMemMapEff

data CpuMemMapEff a where
    MM_Ret :: a -> CpuMemMapEff a
    MM_Bind :: CpuMemMapEff a -> (a -> CpuMemMapEff b) -> CpuMemMapEff b
    MM_Con :: ControllerEff a -> CpuMemMapEff a
    MM_Reg :: RegsEff a -> CpuMemMapEff a
    MM_Ram :: Ram2k.Effect a -> CpuMemMapEff a

instance Functor CpuMemMapEff where fmap = liftM
instance Applicative CpuMemMapEff where pure = return; (<*>) = ap
instance Monad CpuMemMapEff where return = MM_Ret; (>>=) = MM_Bind

iCpuMemMapEff :: ButtonState -> CpuMemMapEff a -> StateT (ControllerState,RegsState) NesRam.Effect a
iCpuMemMapEff buttons = \case
    MM_Ret x -> return x
    MM_Bind e f -> do v <- iCpuMemMapEff buttons e; iCpuMemMapEff buttons (f v)

    MM_Con eff -> do
        StateT $ \(consState,regsState) -> do
            let (v,conState') = runState (iController buttons eff) consState
            return (v,(conState',regsState))

    MM_Reg eff -> do
        StateT $ \(consState,regsState) -> NesRam.InVram $ do
            (regsState',v) <- iRegs eff regsState
            return (v,(consState,regsState'))

    MM_Ram eff -> lift $ NesRam.InWram eff


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

data ControllerEff a where
    ControllerEff_Read :: ControllerEff Byte
    ControllerEff_Strobe :: Bool -> ControllerEff ()

iController :: ButtonState -> ControllerEff a -> State ControllerState a
iController _buttons = \case
    ControllerEff_Strobe bool ->
        state $ \conState -> ((), Controller.strobe bool conState)
    ControllerEff_Read -> do
        state $ \conState -> do
            let (bool,conState') = Controller.read conState
            let byte = if bool then 1 else 0
            (byte, conState')

----------------------------------------------------------------------
-- Regs

type RegsState = PPU.Regs.State

regsState0 :: RegsState
regsState0 = PPU.Regs.init

setVBlank :: RegsState -> Bool -> RegsState
setVBlank = PPU.Regs.setVBlank

isEnabledNMI :: RegsState -> Bool
isEnabledNMI = PPU.Regs.isEnabledNMI

type RegsEff a = PPU.Regs.Effect a

iRegs :: RegsEff a -> RegsState -> Ram2k.Effect (RegsState, a)
iRegs e s = PPU.Regs.run s e
