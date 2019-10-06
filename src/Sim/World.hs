
module Sim.World(
    World(..),
    world0,
    updateWorld,
    ) where

import Data.Set(Set)
import qualified Data.Set as Set

import Nes
import Nes.Emu (neverStopping)
import PPU.Render(Display)
import qualified Controller
import qualified Nes.Emu
import qualified NesRam
import qualified PPU.Render as PPU
import qualified Sim

type Buttons = Set Controller.Button

data World = World
    { frameCount :: Int
    , display :: Display
    , buttons :: Buttons
    , rr :: Nes.RamRom
    , frames :: Sim.Frames Display
    , paused :: Bool
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
    let frames = Sim.frames buttons $ Nes.Emu.interpret rr ns neverStopping
    return $ World { frameCount, display, buttons, rr, frames, paused = False }

updateWorld :: Bool -> Float -> World -> IO World
updateWorld _debug _delta world@World{frameCount,buttons,frames,paused} =
    if paused then return world else do
        (display,frames) <- Sim.unFrames frames buttons
        return $ world { frameCount = frameCount + 1, display, frames }
