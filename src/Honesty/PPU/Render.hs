
module Honesty.PPU.Render(
    Display(..),
    render
    ) where

import Control.Monad(when)

import Honesty.Nes as Nes
import Honesty.PPU.Colour as Colour
import Honesty.PPU.Graphics
import Honesty.PPU.Regs(Control(..),Mask(..))
import qualified Honesty.PPU.Graphics as Graphics
import qualified Honesty.PPU.OAM as OAM
import qualified Honesty.PPU.Palette as Palette
import qualified Honesty.PPU.Regs as Regs
import qualified Honesty.Ram2k as Ram2k

data Display = Display
    { bg :: Colour
    , at :: Screen
    , pf :: Screen
    , spr :: Screen
    , combined :: Screen
    , sprites :: [Sprite]
    , control :: Regs.Control
    , mask :: Regs.Mask
    }

render :: Nes.RamRom -> Regs.State -> Palette.State -> OAM.State -> Ram2k.Effect Display
render Nes.RamRom{pat1,pat2} regs pal oam = do

    let control@Control
            { masterSlave
            , spriteHeight
            --, backgroundTileSelect
            --, spriteTileSelect
            --, nameTableSelect1
            --, nameTableSelect0
            } = Regs.decodeControl regs

    when (masterSlave) $ error "masterSlave/Master"
    when (spriteHeight) $ error "spriteHeight/8x16"

    let mask@Mask
            { blueEmphasis
            , greenEmphasis
            , redEmphasis
            --, spriteEnable
            --, backgroundEnable
            --, spriteLeftColumnEnable
            --, backgroundLeftColumnEnable
            , greyScale
            } = Regs.decodeMask regs

    when (redEmphasis) $ error "redEmphasis"
    when (greenEmphasis) $ error "greenEmphasis"
    when (blueEmphasis) $ error "blueEmphasis"
    when (greyScale) $ error "greyScale"

    let (bg,palettes) = makePalettes pal
    let oamBytes = OAM.contents oam

    kb <- mapM (\a -> Ram2k.Read a) [0..0x3ff]
    --kb2 <- mapM (\a -> Ram2k.Read a) [0x400..0x7ff]

    let sprites = Graphics.seeSprites palettes oamBytes pat1

    let at = Graphics.screenAT bg palettes (drop 960 kb)
    let pf = Graphics.screenPF bg palettes kb pat2
    let spr = Graphics.screenSprites bg palettes oamBytes pat1
    let combined = Graphics.screenCombined bg palettes (oamBytes,pat1) (kb,pat2)

    return $ Display { bg, at, pf, spr, sprites, combined, control, mask }


-- TODO: move this code into Palette module?
makePalettes :: Palette.State -> (Colour,Palettes)
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
            return (bg, Palettes { p0,p1,p2,p3,p4,p5,p6,p7 })
    snd $ Palette.inter pal eff
    where readCol a = do
              b <- Palette.Read a
              return $ Colour.ofByte b
          readPal a = do
              c1 <- readCol a
              c2 <- readCol (a+1)
              c3 <- readCol (a+2)
              return $ Palette { c1,c2,c3 }

