
module Honesty.PPU.Render(
    Display(..),
    render
    ) where

import Control.Monad(when)

import Honesty.Nes as Nes hiding (regs)
import Honesty.PPU.Colour as Colour
import Honesty.PPU.Graphics
import Honesty.PPU.Regs as Regs(State(State,scroll_x,scroll_y),Control(..),Mask(..))
import qualified Honesty.PPU.Graphics as Graphics
import qualified Honesty.PPU.OAM as OAM
import qualified Honesty.PPU.Palette as Palette
import qualified Honesty.PPU.Regs as Regs
import qualified Honesty.Ram2k as Ram2k

data Display = Display
    { frameCount :: Int
    , bg :: Colour
    , pf :: Screen
    , pf2 :: Screen
    , spr :: Screen
    , combined :: Screen
    , sprites :: [Sprite]
    , regs :: Regs.State
    , control :: Regs.Control
    , mask :: Regs.Mask
    }

render :: Int -> Nes.RamRom -> Regs.State -> Palette.State -> OAM.State -> Ram2k.Effect Display
render frameCount Nes.RamRom{pat1,pat2} regs pal oam = do

    let Regs.State{scroll_x,scroll_y} = regs

    let control@Control
            { masterSlave
            , spriteHeight
            , backgroundTileSelect
            , spriteTileSelect
            , nameTableSelect1
            , nameTableSelect0
            } = Regs.decodeControl regs

    when (masterSlave) $ error "masterSlave/Master"
    when (spriteHeight) $ error "spriteHeight/8x16"

    let patB = if backgroundTileSelect then pat2 else pat1
    let patS = if spriteTileSelect then pat2 else pat1

    let mask@Mask
            { blueEmphasis
            , greenEmphasis
            , redEmphasis
            , spriteEnable
            , backgroundEnable
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

    kb1 <- mapM (\a -> Ram2k.Read a) [0..0x3ff]
    kb2 <- mapM (\a -> Ram2k.Read a) [0x400..0x7ff]

    let _ = nameTableSelect1
    let scroll_y_with_NT = scroll_y + if nameTableSelect0 then 240 else 0

    let makeScreen se be kbPair scroll =
            Graphics.screenCombined
            scroll
            bg palettes
            (se,oamBytes,patS)
            (be,kbPair,patB)

    let sprites = Graphics.seeSprites palettes oamBytes patS

    --let at1 = Graphics.screenAT bg palettes (drop 960 kb1)
    --let at2 = Graphics.screenAT bg palettes (drop 960 kb2)

    let no_scroll = (0,0)

    let pf = makeScreen False True (kb1,kb2) no_scroll
    let pf2 = makeScreen False True (kb2,kb1) no_scroll
    let spr = makeScreen True False (kb1,kb2) no_scroll

    let scroll = (scroll_x,scroll_y_with_NT)

    let combined = makeScreen spriteEnable backgroundEnable (kb1,kb2) scroll


    return $ Display { frameCount,
                       bg,
                       pf, pf2, spr,
                       combined,
                       sprites,
                       regs, control, mask
                     }


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

