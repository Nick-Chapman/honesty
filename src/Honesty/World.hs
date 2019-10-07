
module Honesty.World(
    ChooseToDisplay(..),
    World(..),
    world0,
    updateWorld,
    ) where

import Data.Set(Set)
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
    | ChooseAT
    | ChoosePlayfield
    | ChooseOnlySprites
    | ChooseCombined
    deriving (Enum)

data World = World
    { frameCount :: !Int
    , display :: !Display
    , buttons :: !Buttons
    , rr :: !Nes.RamRom
    , frames :: Sim.Frames Display
    , paused :: !Bool
    , chooseL :: [ChooseToDisplay]
    , chooseR :: [ChooseToDisplay]
    , debugSprites :: !Bool
    }

world0 :: String -> IO World
world0 path = do
    (rr,pc0) <- rr0pc0 path
    let ns = state0 pc0
    let Nes.RamRom{ram} = rr
    let Nes.State{regs,pal,oam} = ns
    let frameCount = 0
    display <- NesRam.inter ram $ NesRam.InVram (PPU.render rr regs pal oam)
    let buttons = Set.empty
    let frames = Sim.frames buttons $ Emu.interpret rr ns neverStopping

    let chooseL = cycle [ChooseNothing .. ChooseCombined]
    let chooseR = drop 4 $ cycle [ChooseNothing .. ChooseCombined]

    return $ World { frameCount, display, buttons, rr, frames,
                     paused = False, chooseL, chooseR, debugSprites = False }

    where cycle xs = ys where ys = xs <> ys

updateWorld :: Bool -> Float -> World -> IO World
updateWorld _debug _delta world@World{frameCount,buttons,frames,paused} =
    if paused then return world else do
        (display,frames) <- Sim.unFrames frames buttons
        return $ world { frameCount = frameCount + 1, display, frames }
