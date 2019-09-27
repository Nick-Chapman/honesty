
module System(
    --State(..),
    --CHR,
    --state0,
    --stepCPU,
    --stepNMI,
    --renderScreen,
    ) where

{-
import NesFile(NesFile(..),loadNesFile)
import Graphics(CHR,Screen)

import Six502.Values
import Six502.Decode(decode1)
import Six502.Disassembler(ljust,displayOpLine)
import Six502.Emu(Cycles)

import qualified Six502.Emu as Six
import qualified Six502.Mem as Six.Mem
import qualified Ram2k
import qualified PPU.Regs as Regs
import qualified PPU.Render
import qualified PRG
import qualified Controller as Con

import Six502.Emu(Cpu)

data State = State
    { ro :: Six.Mem.RO
    , wram :: Ram2k.State
    , ppu_regs :: Regs.State
    , cpu :: Cpu
    , cc :: Cycles
    , chr1 :: CHR
    , chr2 :: CHR
    , vram :: Ram2k.State
    , con :: Con.State
    }

instance Show State where
    show state = do
        let State{ro,wram,cpu,ppu_regs,cc,vram,con} = state
        let Six.Cpu{Six.pc} = cpu
        let (_,(_,(_,bytes))) = Ram2k.run vram (Regs.run ppu_regs (Six.Mem.run ro (con,wram) (Six.Mem.reads pc)))
        let op = decode1 bytes
        let col = 48
        ljust col (displayOpLine pc op) <> show cpu  <> " " <> show cc

stepCPU :: State -> State
stepCPU state = do
    let State{ro,wram,cpu,ppu_regs,cc,vram,con} = state
    let mem_eff = Six.stepInstruction cpu
    let ppu_regs_eff = Six.Mem.run ro (con,wram) mem_eff
    let vram_eff :: Ram2k.Effect (Regs.State,((Con.State,Ram2k.State),(Cpu, Cycles))) = Regs.run ppu_regs ppu_regs_eff
    let (vram1,(ppu_regs1,((con1,wram1),(cpu1,n)))) = Ram2k.run vram vram_eff
    state { wram = wram1, cpu = cpu1, ppu_regs = ppu_regs1, cc = cc + n
          , vram = vram1
          , con = con1
          }

stepNMI :: State -> State
stepNMI state = do
    let State{ro,wram,cpu,ppu_regs,vram,con} = state
    let mem_eff = Six.triggerNMI cpu
    let ppu_regs_eff = Six.Mem.run ro (con,wram) mem_eff
    let vram_eff = Regs.run ppu_regs ppu_regs_eff
    let (vram1,(ppu_regs1,((con1,wram1),cpu1))) = Ram2k.run vram vram_eff
    let _ignore = (vram1,ppu_regs1,con1) -- ok to ignore these effects, which can/should not happen?
    state { wram = wram1, cpu = cpu1 }

renderScreen :: State -> Screen
renderScreen state = do
    let State{vram,ppu_regs,chr1,chr2} = state
    PPU.Render.render vram ppu_regs (chr1,chr2)

state0 :: String -> IO State
state0 path = do
    nesfile@NesFile{chrs=[(chr1,chr2)]} <- loadNesFile path
    let (ro,prg) = roOfNesFile nesfile
    let pc0 = resetAddr path prg
    return $ State
        { ro
        , wram = Ram2k.init "wram"
        , ppu_regs = Regs.init
        , cpu = Six.cpu0 pc0
        , cc = 7  -- from nestest.log
        , chr1
        , chr2
--        , vram = _fillRamWithCrap $ Ram2k.init "vram"
        , vram = Ram2k.init "vram"
        , con = Con.init
        }

_fillRamWithCrap :: Ram2k.State -> Ram2k.State
_fillRamWithCrap ram =
    fst $ Ram2k.run ram $ mapM_ (\a -> Ram2k.Write a $ fromIntegral a) [0x0..0x7ff]


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
-}
