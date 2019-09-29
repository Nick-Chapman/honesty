
module Top(gloss,
           printRun,
           Model(..),
           model0,
           pictureModel,
           handleEventModel,
           updateModel,
           ) where

import Control.Monad (ap,liftM)
import Control.Monad (forever)
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad.State
import Text.Printf(printf)

import Graphics.Gloss (translate,scale,pictures,color,cyan,Picture(..))
import Graphics.Gloss.Interface.IO.Game(Event(..),Key(..),SpecialKey(..),KeyState(..))
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import qualified Graphics(pictureScreen,screenBG)
import Graphics(Screen,PAT,Palette(..),Palettes(..))

import Nes
import NesFile
import PPU.Colour
import Six502.Cycles
import Six502.Decode(decode1)
import Six502.Disassembler(ljust,displayOpLine)
import Six502.Values
import qualified CHR
import qualified Controller
import qualified NesRam
import qualified PPU.Palette as Palette
import qualified PPU.Regs as Regs
import qualified PRG
import qualified Ram2k
import qualified Six502.Cpu
import qualified Six502.Emu
import qualified Six502.MM as MM
import qualified Six502.Mem

printNS :: NesRamRom -> Nes.State -> IO ()
printNS rr ns@Nes.State{cpu,cc,regs=_} = do
    let Six502.Cpu.State{Six502.Cpu.pc} = cpu
    bytes <- readFromAddr ns rr pc
    let op = Six502.Decode.decode1 bytes
    let col = 48
    let s = ljust col (displayOpLine pc op) <> show cpu  <> " " <> show cc
    putStrLn s

gloss :: String -> Bool -> Int -> IO ()
gloss path fs sc = do
    let debug = True
    model <- model0 path
    Gloss.playIO dis (Gloss.greyN 0.3) fps model
        (\  m -> return $ doPosition (pictureModel m))
        (\e m -> handleEventModel e m)
        (\d m -> updateModel debug d m)
    where
        dis = if fs
              then Gloss.FullScreen
              else Gloss.InWindow "NES" (sc * x,sc * y) (0,0)

        fps = 20 -- 60

        doPosition = doScale . doBorder . doTransOriginUL
        doScale = scale (fromIntegral sc) (fromIntegral sc)

        doBorder = translate 10 (-10)
        doTransOriginUL = translate (- ((fromIntegral x)/2)) ( ((fromIntegral y)/2))

        x = 800
        y = 400


type Buttons = Set Controller.Button

-- TODO: rename as World
data Model = Model
    { frameCount :: Int
    , display :: Display
    , sys :: Sys
    , buttons :: Buttons
    , rr :: NesRamRom
    }

-- TODO: collect/isolate all Gloss using code
pictureModel :: Model -> Gloss.Picture
pictureModel Model{frameCount,rr,display,buttons} = pictures
    [ scale 1 (-1) $ makePicture rr display
    , translate 0 (-380) $ scale 0.5 0.5 $ color cyan $ Text (show frameCount)
    , translate 150 (-380) $ scale 0.5 0.5 $ color cyan $ Text (Controller.showPressed buttons)
    ]

handleEventModel :: Gloss.Event -> Model -> IO Model
handleEventModel event model@Model{buttons} = do
    --print event
    return $ case event of
        EventKey (SpecialKey KeyEsc) Down _ _ -> error "quit"
        EventKey (Char 'z') ud _ _ -> joy ud Controller.A
        EventKey (Char 'x') ud _ _ -> joy ud Controller.B
        EventKey (SpecialKey KeyTab) ud _ _ -> joy ud Controller.Select
        EventKey (SpecialKey KeyEnter) ud _ _ -> joy ud Controller.Start
        EventKey (SpecialKey KeyUp) ud _ _ -> joy ud Controller.Up
        EventKey (SpecialKey KeyDown) ud _ _ -> joy ud Controller.Down
        EventKey (SpecialKey KeyLeft) ud _ _ -> joy ud Controller.Left
        EventKey (SpecialKey KeyRight) ud _ _ -> joy ud Controller.Right
        _ -> model
  where
        joy = \case Down -> press; Up -> release
        press but = model { buttons = Set.insert but buttons }
        release but = model { buttons = Set.delete but buttons }


_showDelta :: Float -> String
_showDelta = printf "%.03g"

-- TODO: updateModel & printRun functions should becombined

updateModel :: Bool -> Float -> Model -> IO Model
updateModel _debug _delta model@Model{frameCount,sys,buttons} = loop sys
    where
        loop :: Sys -> IO Model
        loop = \case
            Sys_Render nesState display sysIO -> do
                let Nes.State{cc=_cc,pal=_pal} = nesState
                when _debug $ print (_showDelta _delta, frameCount,_cc)
                sys <- sysIO
                -- TODO: force to WHNF here?
                return $ model { frameCount = frameCount + 1, display, sys }
            Sys_Log (Log_NesState _nesState) sysIO -> do
                sys <- sysIO
                loop sys
            Sys_Log Log_NMI sysIO -> do
                --print "--------------------NMI--------------------"
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
            Sys_Render _ _ sysIO -> do
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
--makePicture NesRamRom{pat1,pat2} Display{bg1,bg2} = do
makePicture _ Display{bg1} = do
    --let _ = screenTiles
    --let left = screenTiles pat1
    --let right = screenTiles pat2
    pictures
        [ Graphics.pictureScreen bg1
        --, translate 300 0 $ pictureScreen bg2
        --, translate 600 0 $ pictureScreen left
        --, translate 600 150 $ pictureScreen right
        ]


model0 :: String -> IO Model
model0 path = do
    nesfile@NesFile{pats=[(pat1,pat2)]} <- loadNesFile path
    let prg = prgOfNesFile nesfile
    let chr = chrOfNesFile nesfile
    let pc0 = resetAddr path prg
    ram <- NesRam.newMState
    let rr = NesRamRom { ram, prg, chr, pat1, pat2 }
    let ns@Nes.State{regs,pal} = Nes.state0 pc0
    sys <- sysOfNesState rr ns
    display <- NesRam.inter ram $ NesRam.InVram (render rr regs pal)
    return $ Model { frameCount = 0
                   , display
                   , sys
                   , buttons = Set.empty
                   , rr
                   }

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

data NesRamRom = NesRamRom
    { ram :: NesRam.MState,
      prg :: PRG.ROM,
      chr :: CHR.ROM,
      pat1 :: PAT,
      pat2 :: PAT
    }

sysOfNesState :: NesRamRom -> Nes.State -> IO Sys
sysOfNesState rr nesState = do
    let finished (_::Nes.State) () = error "we finished"
    interpretStep rr nesState nesForever finished

cyclesPerFrame :: Cycles
cyclesPerFrame = 29780

cyclesInVBlank :: Cycles
cyclesInVBlank = fromIntegral ((8200::Int) `div` 3)

framesUntilPPuWarmsUp :: Int
framesUntilPPuWarmsUp = 1

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
    Log :: String -> Step ()

instance Functor Step where fmap = liftM
instance Applicative Step where pure = return; (<*>) = ap
instance Monad Step where return = Step_Ret; (>>=) = Step_Bind

data Logged = Log_NesState Nes.State | Log_NMI | Log_Info String

-- TODO: this continutaion based type is overly complex
data Sys
    = Sys_Render Nes.State Display (IO Sys)
    | Sys_Log Logged (IO Sys)
    | Sys_SampleButtons (Buttons -> IO Sys)

interpretStep :: NesRamRom -> Nes.State -> Step a -> (Nes.State -> a -> IO Sys) -> IO Sys
interpretStep rr@NesRamRom{ram,prg} s@Nes.State{regs,pal} step k = case step of
    Step_Ret x -> k s x
    Step_Bind e f -> interpretStep rr s e $ \s v -> interpretStep rr s (f v) k
    SetVBlank bool -> do
        let s' = s {regs = Regs.setVBlank regs bool}
        k s' ()
    Render -> do
        display <- NesRam.inter ram $ NesRam.InVram (render rr regs pal)
        return $ Sys_Render s display $ k s ()
    Buttons ->
        return $ Sys_SampleButtons $ \buttons -> k s buttons
    RunCpuInstruction buttons -> do
        return $ Sys_Log (Log_NesState s) $ do
            (s',cycles) <- NesRam.inter ram (cpuInstruction rr prg buttons s)
            k s' cycles
    IsNmiEnabled -> do
        let e = Regs.isEnabledNMI regs
        k s e
    TriggerNMI -> do
        return $ Sys_Log (Log_NesState s) $ return $ Sys_Log Log_NMI $ do
            s' <- NesRam.inter ram $ triggerNMI rr prg s
            k s' ()
    Log info ->
        return $ Sys_Log (Log_Info info) $ do
            k s ()


-- TODO: move this to new module: PPU.Render

render :: NesRamRom -> Regs.State -> Palette.State -> Ram2k.Effect Display
render NesRamRom{pat1,pat2} _regs pal = do
    -- Depending on nametable mirroring (V/H) as selected in PPUCTRL
    -- shoud read 1st or 2nd K of the vram.
    -- Probably better to go via the memory map!

    kilobyte1 <- mapM (\a -> Ram2k.Read a) [0..0x3ff]
    kilobyte2 <- mapM (\a -> Ram2k.Read a) [0x400..0x7ff]

    -- depending on some other flag in the Regs,
    -- should pick pat1 or pat2
    let palettes = makePalettes pal
    let patPick = True
    let pat = if patPick then pat2 else pat1
    let bg1 = Graphics.screenBG palettes kilobyte1 pat
    let bg2 = Graphics.screenBG palettes kilobyte2 pat
    let display = Display { bg1, bg2 }
    return $ display


-- TODO: move this code into Palette module
makePalettes :: Palette.State -> Palettes
makePalettes pal = do
    let eff = do
            bg <- readCol 0
            p1 <- readPal 1
            p2 <- readPal 5
            p3 <- readPal 9
            p4 <- readPal 13
            return $ Palettes { p1,p2,p3,p4, bg }
    snd $ Palette.inter pal eff
    where readCol a = do
              b <- Palette.Read a
              return $ PPU.Colour.ofByte b
          readPal a = do
              c1 <- readCol a
              c2 <- readCol (a+1)
              c3 <- readCol (a+2)
              return $ Palette { c1,c2,c3 }


-- TODO: move this code into Six502.Emu. Factor common code from the 3 defs

cpuInstruction :: NesRamRom -> PRG.ROM -> Buttons -> Nes.State -> NesRam.Effect (Nes.State,Cycles)
cpuInstruction NesRamRom{chr} prg2 buttons ns@Nes.State{cpu,cc} = do
    let opPrg1 = Nothing-- TODO
    let mm_eff = Six502.Mem.inter (opPrg1,prg2) (Six502.Emu.stepInstruction cpu)
    do
        ((cpu',cycles),ns') <- runStateT (MM.inter cc chr buttons mm_eff) ns
        let ns'' = ns' { cpu = cpu', cc = cc+cycles }
        return (ns'',cycles)

triggerNMI :: NesRamRom -> PRG.ROM -> Nes.State -> NesRam.Effect Nes.State
triggerNMI NesRamRom{chr} prg2 ns@Nes.State{cpu,cc} = do
    let opPrg1 = Nothing-- TODO
    let mm_eff = Six502.Mem.inter (opPrg1,prg2) (Six502.Emu.triggerNMI cpu)
    let buttons = Set.empty -- ok?
    do
        (cpu',ns') <- runStateT (MM.inter cc chr buttons mm_eff) ns
        return ns' { cpu = cpu' }

readFromAddr :: Nes.State -> NesRamRom -> Addr -> IO [Byte]
readFromAddr ns@Nes.State{cc} NesRamRom{prg,chr,ram} pc = do
    let opPrg1 = Nothing-- TODO
    let mem_eff = Six502.Mem.reads pc
    let _tag = "readFromAddr:"<>show ns
    let mm_eff = Six502.Mem.inter  (opPrg1,prg) mem_eff
    let buttons = Set.empty
    (bytes,_) <- NesRam.inter ram $ runStateT (MM.inter cc chr buttons mm_eff) ns
    return bytes
