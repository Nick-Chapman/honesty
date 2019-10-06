
module PPU.Render(
    Display(..),
    render
    ) where

import Nes
import PPU.Graphics
import PPU.Colour
import qualified PPU.Graphics as Graphics
import qualified Ram2k
import qualified PPU.Regs as Regs
import qualified PPU.OAM as OAM
import qualified PPU.Palette as Palette

data Display = Display
    { tiles1 :: Screen
    , tiles2 :: Screen
    , bg1 :: Screen
    , bg2 :: Screen
    , at1 :: Screen
    , at2 :: Screen
    , pals :: [Screen]
    , sprites :: [Sprite]
    , spr :: Screen
    }

render :: Nes.RamRom -> Regs.State -> Palette.State -> OAM.State -> Ram2k.Effect Display
render Nes.RamRom{pat1,pat2} _regs pal oam = do

    let palettes = makePalettes pal
    let tiles1 = Graphics.screenTiles pat1
    let tiles2 = Graphics.screenTiles pat2

    kilobyte1 <- mapM (\a -> Ram2k.Read a) [0..0x3ff]
    kilobyte2 <- mapM (\a -> Ram2k.Read a) [0x400..0x7ff]

    let bg1 = Graphics.screenBG palettes kilobyte1 pat2
    let bg2 = Graphics.screenBG palettes kilobyte2 pat2
    let at1 = Graphics.screenAT palettes (drop 960 kilobyte1)
    let at2 = Graphics.screenAT palettes (drop 960 kilobyte2)

    let pals = Graphics.screenPalettes palettes
    let sprites = Graphics.seeSprites palettes (OAM.contents oam) pat1

    let spr = Graphics.screenSprites palettes (OAM.contents oam) pat1

    let display = Display { bg1, bg2, tiles1, tiles2, at1, at2, pals, sprites, spr }
    return $ display


-- TODO: move this code into Palette module?
makePalettes :: Palette.State -> Palettes
makePalettes pal = do
    let eff = do
            bg <- readCol 0
            p0 <- readPal 1
            p1 <- readPal 5
            p2 <- readPal 9
            p3 <- readPal 0xD
            p4 <- readPal 0x11
            p5 <- readPal 0x15
            p6 <- readPal 0x19
            p7 <- readPal 0x1D
            return $ Palettes { bg, p0,p1,p2,p3,p4,p5,p6,p7 }
    snd $ Palette.inter pal eff
    where readCol a = do
              b <- Palette.Read a
              return $ PPU.Colour.ofByte b
          readPal a = do
              c1 <- readCol a
              c2 <- readCol (a+1)
              c3 <- readCol (a+2)
              return $ Palette { c1,c2,c3 }

