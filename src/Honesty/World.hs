
module Honesty.World(
    ChooseToDisplay(..),
    World(..),
    world0,
    updateWorld,
    ) where

import Data.Fixed(Fixed(..),HasResolution,resolution)
import Data.Time (UTCTime,getCurrentTime,diffUTCTime,nominalDiffTimeToSeconds)
import Data.Set(Set)
import Text.Printf (printf)
import qualified Data.Set as Set

import Honesty.Emu (neverStopping)
import Honesty.Nes as Nes
import Honesty.PPU.Render(Display)
import qualified Honesty.Controller as Controller
import qualified Honesty.Emu as Emu
import qualified Honesty.NesRam as NesRam
import qualified Honesty.PPU.Render as PPU
import qualified Honesty.Sim as Sim

type Buttons = Set Controller.Button

data ChooseToDisplay
    = ChooseNothing
    | ChoosePlayfield
    | ChooseOnlySprites
    | ChooseFullGame
    deriving (Enum)

data World = World
    { time :: UTCTime
    , fps :: Fps
    , display :: !Display
    , buttons :: !Buttons
    , rr :: !Nes.RamRom
    , frames :: Sim.Frames Display
    , paused :: !Bool
    , chooseL :: [ChooseToDisplay]
    , chooseR :: [ChooseToDisplay]
    , debugSprites :: !Bool
    , debugFrames :: !Bool
    , debugButtons :: !Bool
    , debugRegs :: !Bool
    }

world0 :: String -> Bool -> IO World
world0 path debug = do
    let trace = False
    (rr,pc0) <- rr0pc0 path
    let ns = state0 pc0
    let Nes.RamRom{ram} = rr
    let Nes.State{regs,pal,oam,fn,cc} = ns
    display <- NesRam.inter debug fn cc ram $ NesRam.InVram (PPU.render fn rr regs pal oam)
    let buttons = Set.empty
    let frames = Sim.frames trace rr buttons $ Emu.interpret debug rr ns neverStopping
    let chooseL = cycle [ChooseNothing .. ChooseFullGame]
    let chooseR = ChooseFullGame : cycle [ChooseNothing .. ChooseFullGame]
    time <- getCurrentTime
    return $ World { time
                   , fps = Fps $ 0
                   , display , buttons, rr, frames
                   , paused = False
                   , chooseL
                   , chooseR
                   , debugSprites = False
                   , debugFrames = True
                   , debugButtons = True
                   , debugRegs = True
                   }

    where cycle xs = ys where ys = xs <> ys

updateWorld :: World -> IO World
updateWorld world@World{time,fps,buttons,frames,paused} =
    if paused then return world else do
        (display,frames) <- Sim.unFrames frames buttons
        time' <- getCurrentTime
        let fpsNow = makeFps 1 time time'
        let fps' = smoothFps fps fpsNow
        return $ world { time = time', fps = fps', display, frames }


newtype Fps = Fps Float

instance Show Fps where show (Fps f) = printf "%.01g" f

smoothFps :: Fps -> Fps -> Fps
smoothFps (Fps prev) (Fps next) = Fps $ prev * decay + next * (1 - decay)
    where decay = 0.99


makeFps :: Int -> UTCTime -> UTCTime -> Fps
makeFps frames a b = do
    let nd = diffUTCTime b a
    let f = fromFixed $ nominalDiffTimeToSeconds nd
    Fps (fromIntegral frames / f)

-- | Convert From Fixed to Fractional
fromFixed :: (Fractional a, HasResolution b) => Fixed b -> a
fromFixed fv@(MkFixed v) = (fromIntegral v) / (fromIntegral $ resolution fv)
