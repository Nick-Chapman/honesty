
module PPU.Render(
    render
    ) where

import Six502.Values
import Ram2k
import PPU.Regs
import Graphics(CHR,Screen,screenBG)

render :: Ram2k.State -> PPU.Regs.State -> (CHR,CHR) -> Graphics.Screen
render vram _ppu_regs (chr1,chr2) = do

    -- Depending on nametable mirroring (V/H) as selcte by PPU_Regs
    -- shoud read 1st or 2nd K of the vram.
    -- Probably better to go via the memory map!
    let ntPick = False
    let kilobyte = if ntPick then readK2 vram else readK1 vram

    -- depending on some other flag in the PPU.Regs,
    -- should pick chr1 or chr2
    let chrPick = True
    let chr = if chrPick then chr2 else chr1
    screenBG kilobyte chr

readK1 :: Ram2k.State -> [Byte]
readK1 ram  =
    snd $ Ram2k.run ram $ mapM (\a -> Ram2k.Read a) [0..0x3ff]

readK2 :: Ram2k.State -> [Byte]
readK2 ram  =
    snd $ Ram2k.run ram $ mapM (\a -> Ram2k.Read a) [0x400..0x7ff]
