
module System(
    emu,
    State(..),CHR, state0, step
    ) where

import NesFile(NesFile(..),loadNesFile)
import Graphics(CHR)

import Six502.Values
import Six502.Decode(decode1)
import Six502.Disassembler(ljust,displayOpLine)
import Six502.Emu(Cycles)

import qualified Six502.Emu as Six
import qualified Six502.Mem as Six.Mem
import qualified Ram2k
import qualified PPU.Regs
import qualified PRG

-- simple, non graphical entry point, used for nestest.nes regression test
emu :: String -> IO ()
emu path = do
    state <- state0 path
    let states :: [State] = limitEmuSteps path $ run state
    mapM_ print states
  where
    run :: State -> [State]
    run state = state : run (step state)

    limitEmuSteps :: String -> [a] -> [a]
    limitEmuSteps path =
        case path of
            "data/nestest.nes" -> take 5828 -- until reach unimplemented DCP
            _ -> id

data State = State
    { ro :: Six.Mem.RO
    , wram :: Ram2k.State
    , ppu_regs :: PPU.Regs.State
    , cpu :: Six.Cpu
    , cc :: Cycles
    , chr1 :: CHR
    , chr2 :: CHR
    }

instance Show State where
    show state = do
        let State{ro,wram,cpu,ppu_regs,cc} = state
        let Six.Cpu{Six.pc} = cpu
        let (_,(_,bytes)) = PPU.Regs.run ppu_regs (Six.Mem.run ro wram (Six.Mem.reads pc))
        let op = decode1 bytes
        let col = 48
        ljust col (displayOpLine pc op) <> show cpu  <> " " <> show cc

step :: State -> State
step state = do
    let State{ro,wram,cpu,ppu_regs,cc} = state
    let mem_eff = Six.interpret Six.fetchDecodeExec cpu
    let ppu_regs_eff = Six.Mem.run ro wram mem_eff
    let (ppu_regs1,(wram1,(cpu1,n))) = PPU.Regs.run ppu_regs ppu_regs_eff
    state { wram = wram1, cpu = cpu1, ppu_regs = ppu_regs1, cc = cc + n}
    -- TODO: need/run: PPU, PPU-Mem, renderer, controller

state0 :: String -> IO State
state0 path = do
    nesfile@NesFile{chrs=[(chr1,chr2)]} <- loadNesFile path
    let (ro,prg) = roOfNesFile nesfile
    let pc0 = resetAddr path prg
    return $ State
        { ro
        , wram = Ram2k.init "wram"
        , ppu_regs = PPU.Regs.init
        , cpu = Six.cpu0 pc0
        , cc = 7  -- from nestest.log
        , chr1
        , chr2
        }

roOfNesFile :: NesFile -> (Six.Mem.RO, PRG.ROM)
roOfNesFile NesFile{prgs} =
    case prgs of
        [prg] -> (Six.Mem.rom1 prg, prg)
        [prg1,prg2] -> (Six.Mem.rom2 prg1 prg2, prg2)
        _  ->
            error "emu, unexpected number of prg"

resetAddr :: String -> PRG.ROM -> Addr
resetAddr path prg = do
    let bytes = PRG.unROM prg
    case path of
        "data/nestest.nes" -> 0xC000
        _ -> addrOfHiLo hi lo
            where
                lo = bytes !! 0x3ffc
                hi = bytes !! 0x3ffd
