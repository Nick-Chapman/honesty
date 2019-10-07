
module Honesty.Nes(
    State(..), state0,
    RamRom(..), rr0pc0,
    ) where

import Data.Tuple.Extra((***))

import Honesty.Addr
import Honesty.Byte
import Honesty.NesFile
import Honesty.Six502.Cycles
import qualified Honesty.CHR as CHR
import qualified Honesty.Controller as Controller
import qualified Honesty.NesRam as NesRam
import qualified Honesty.PPU.Graphics as Graphics
import qualified Honesty.PPU.OAM as OAM
import qualified Honesty.PPU.Palette as Palette
import qualified Honesty.PPU.Regs as Regs
import qualified Honesty.PRG as PRG
import qualified Honesty.Six502.Cpu as Cpu

data State = State
    { cpu :: !Cpu.State
    , con :: !Controller.State
    , regs :: !Regs.State
    , cc :: !Cycles
    , pal :: !Palette.State
    , oam :: !OAM.State
    }
    deriving (Show)

state0 :: Addr -> State
state0 pc0 = State
    { cpu = Cpu.state0 pc0
    , con = Controller.state0
    , regs = Regs.state0
    , cc = 7 -- for nestest.nes
    , pal = Palette.state0
    , oam = OAM.state0
    }

data RamRom = RamRom
    { ram :: !NesRam.MState,
      prg :: !PRG.ROM,
      chr :: !CHR.ROM,
      pat1 :: !Graphics.PAT,
      pat2 :: !Graphics.PAT
    }

rr0pc0 :: String -> IO (RamRom, Addr)
rr0pc0 path = do
    nesfile <- loadNesFile path
    let prg = prgOfNesFile nesfile
    let chr = chrOfNesFile nesfile
    let pc0 = resetAddr path prg
    ram <- NesRam.newMState
    let (pat1,pat2) = patPairFromBS (CHR.bytes chr)
    let rr = RamRom { ram, prg, chr, pat1, pat2 }
    return (rr,pc0)

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
