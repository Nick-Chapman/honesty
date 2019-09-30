
module Nes(
    State(..), state0,
    RamRom(..),
    ) where

import Addr
import Six502.Cycles
import qualified Six502.Cpu as Cpu
import qualified Controller
import qualified PPU.Regs as Regs
import qualified PPU.Palette as Palette
import qualified PRG
import qualified NesRam
import qualified CHR
import qualified PPU.Graphics as Graphics

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

data RamRom = RamRom
    { ram :: NesRam.MState,
      prg :: PRG.ROM,
      chr :: CHR.ROM,
      pat1 :: Graphics.PAT,
      pat2 :: Graphics.PAT
    }
