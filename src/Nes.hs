
module Nes(
    State(..), state0,
    ) where

import Six502.Cycles
import Six502.Values
import qualified Six502.Cpu as Cpu
import qualified Controller
import qualified PPU.Regs as Regs
import qualified PPU.Palette as Palette

data State = State
    { cpu :: Cpu.State
    , con :: Controller.State
    , regs :: Regs.State
    , cc :: Cycles
    , pal :: Palette.State
    }
    deriving (Show)

state0 :: Addr -> Nes.State
state0 pc0 = Nes.State
    { cpu = Cpu.state0 pc0
    , con = Controller.state0
    , regs = Regs.state0
    , cc = 7 -- for nestest.nes
    , pal = Palette.state0
    }
