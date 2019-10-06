
module PPU.Graphics(
    PAT, patFromBS,
    Screen, forceScreen,
    screenToBitmapByteString,screenWidth,screenHeight,
    screenTiles,
    screenBG,
    Palettes(..),Palette(..),
    screenAT,
    screenPalettes,
    Sprite(..), Priority(..), seeSprites,
    screenSprites,
    screenCombined,
    ) where

import Data.Array
import Data.Bits
import Data.Maybe (listToMaybe)
import qualified Data.List as List
import qualified Data.ByteString as BS

import Byte
import PPU.Colour as Colour

newtype PAT = PAT (Array Int Byte)

patFromBS :: [Byte] -> PAT
patFromBS = PAT . listArray (0,4095)

mkBS :: [Colour] -> BS.ByteString
mkBS cols = BS.pack $ map fromIntegral $ concat $ do
    col <- cols
    let (r,g,b) = Colour.toRGB col
    return [r,g,b,255]

data Screen = Screen {height,width :: Int, cols :: [Colour] }

screenToBitmapByteString :: Screen -> BS.ByteString
screenToBitmapByteString Screen{cols} = mkBS cols

screenWidth :: Screen -> Int
screenWidth Screen{width} = width

screenHeight :: Screen -> Int
screenHeight Screen{height} = height

forceScreen :: Screen -> Int
forceScreen Screen{cols} = do
    let xs = map fromIntegral $ concat $ do
            col <- cols
            let (r,g,b) = Colour.toRGB col
            return [r,g,b]
    List.foldr (+) 0 xs

data ColourSelect = BG | CS1 | CS2 | CS3
data PaletteSelect = Pal0 | Pal1 | Pal2 | Pal3 | Pal4 | Pal5 | Pal6 | Pal7
data Palette = Palette { c1,c2,c3 :: Colour }
data Palettes = Palettes { bg :: Colour, p0,p1,p2,p3,p4,p5,p6,p7 :: Palette }

selectColour :: Colour -> Palette -> ColourSelect -> Colour
selectColour bg Palette{c1,c2,c3} = \case
    BG -> bg
    CS1 -> c1
    CS2 -> c2
    CS3 -> c3

selectPalette :: Palettes -> PaletteSelect -> Palette
selectPalette Palettes{p0,p1,p2,p3,p4,p5,p6,p7} = \case
    Pal0 -> p0
    Pal1 -> p1
    Pal2 -> p2
    Pal3 -> p3
    Pal4 -> p4
    Pal5 -> p5
    Pal6 -> p6
    Pal7 -> p7

data Priority = InFront | Behind
data Sprite = Sprite { screen :: Screen, x,y::Int, priority :: Priority }

seeSprites :: Palettes -> [Byte] -> PAT -> [Sprite]
seeSprites pals oamBytes pat = do
    let oam = listArray (0,255) oamBytes
    i <- [0..63::Int]
    let sprite@Sprite{y} = seeSprite pals (oam!(4*i), oam!(4*i+1), oam!(4*i+2), oam!(4*i+3)) pat
    if y < 0xEF then [sprite] else []

seeSprite :: Palettes -> (Byte,Byte,Byte,Byte) -> PAT -> Sprite
seeSprite pals (b0,b1,b2,b3) (PAT pt) = do
    let y = byteToUnsigned b0
    let x = byteToUnsigned b3
    let priority = if b2 `testBit` 5 then Behind else InFront

    let flipV = b2 `testBit` 7
    let flipH = b2 `testBit` 6

    let atBitA = b2 `testBit` 1
    let atBitB = b2 `testBit` 0
    let pSel =
            if atBitA
            then (if atBitB then Pal7 else Pal6)
            else (if atBitB then Pal5 else Pal4)
    let pal = selectPalette pals pSel
    let Palettes{bg} = pals
    let cols = do
            y <- (if flipV then reverse else id) [0..7]
            let pti = 16 * byteToUnsigned b1 + y
            let tileByteA = pt ! pti
            let tileByteB = pt ! (pti+8)
            x <- (if flipH then reverse else id) [0..7]
            let tileBitA = tileByteA `testBit` (7 - x)
            let tileBitB = tileByteB `testBit` (7 - x)
            let cSel =
                    if tileBitA
                    then (if tileBitB then CS3 else CS2)
                    else (if tileBitB then CS1 else BG)
            return $ selectColour bg pal cSel

    let screen = Screen { height = 8, width = 8, cols }
    Sprite { screen, x, y, priority }


screenTiles :: PAT -> Screen -- see all 256 tiles in the PAT
screenTiles (PAT pt) = do
    let (bg,somePalette) = do
            let black = Colour.ofByte 0x0F
            let red = Colour.ofByte 0x06
            let green = Colour.ofByte 0x2a
            let white = Colour.ofByte 0x20
            (black, Palette { c1 = red, c2 = green, c3 = white })
    let cols = do
            y6543 <- [0..15::Int]
            y210 <- [0..7::Int]
            x6543 <- [0..15::Int]
            let ti = 16 * y6543 + x6543
            let pti = 16*ti + y210
            let tileByteA = pt ! pti
            let tileByteB = pt ! (pti+8)
            x210 <- [0..7::Int]
            let tileBitA = tileByteA `testBit` (7 - x210)
            let tileBitB = tileByteB `testBit` (7 - x210)
            let cSel =
                    if tileBitA
                    then (if tileBitB then CS3 else CS2)
                    else (if tileBitB then CS1 else BG)
            return $ selectColour bg somePalette cSel
    Screen { height = 128, width = 128, cols}


_opt_screenBG :: Palettes -> [Byte] -> PAT -> Screen
_opt_screenBG pals kb (PAT pt) = do
    let Palettes{bg} = pals
    let nt = listArray (0,959) (take 960 kb)
    let at = listArray (0,63) (drop 960 kb)
    let cols = do
            y76543 <- [0..29::Int]
            let y4 = y76543 `testBit` 1
            let y765 = y76543 `div` 4
            yMod8 <- [0..7::Int]
            x765 <- [0..7]
            let ati = 8 * y765 + x765
            let atByte  = at ! ati
            x43 <- [0..3::Int]
            let x76543 = x765 * 4 + x43
            let x4 = x43 `testBit` 1
            let quad = if y4 then (if x4 then 3 else 2) else (if x4 then 1 else 0) -- 0..3
            let atBitA = atByte `testBit` (2*quad + 1)
            let atBitB = atByte `testBit` (2*quad)
            let pSel =
                    if atBitA
                    then (if atBitB then Pal3 else Pal2)
                    else (if atBitB then Pal1 else Pal0)
            let pal = selectPalette pals pSel
            let nti = 32 * y76543 + x76543
            let ti = fromIntegral $ unByte $ nt ! nti
            let pti = 16*ti + yMod8
            let tileByteA = pt ! (pti+8)
            let tileByteB = pt ! pti
            xMod8 <- [0..7::Int]
            let tileBitA = tileByteA `testBit` (7 - xMod8)
            let tileBitB = tileByteB `testBit` (7 - xMod8)
            let cSel =
                    if tileBitA
                    then (if tileBitB then CS3 else CS2)
                    else (if tileBitB then CS1 else BG)
            return $ selectColour bg pal cSel
    Screen { height = 240, width = 256, cols }


screenPalettes :: Palettes -> [Screen]
screenPalettes (Palettes {bg,p0,p1,p2,p3}) = map (screenPalette bg) [p0,p1,p2,p3]

screenPalette :: Colour -> Palette -> Screen
screenPalette bg pal = do
    let Palette { c1,c2,c3 } = pal
    let cols = do
            y <- [0..15::Int]
            x <- [0..15::Int]
            let x3 = x `testBit` 3
            let y3 = y `testBit` 3
            let col =
                    if y3
                    then (if x3 then c3 else c2)
                    else (if x3 then c1 else bg)
            return col
    Screen { height = 16, width = 16, cols }


screenAT :: Palettes -> [Byte] -> Screen
screenAT pals atBytes = do
    let Palettes{bg} = pals
    let at = listArray (0,63::Int) atBytes
    let cols = do
            y <- [0..239]
            x <- [0..255]
            let ati = 8*(y`div`32) + x`div`32
            let atByte  = at ! ati
            let x4 = x `testBit` 4
            let y4 = y `testBit` 4
            let quad = if y4 then (if x4 then 3 else 2) else (if x4 then 1 else 0) -- 0..3
            let atBitA = atByte `testBit` (2*quad + 1)
            let atBitB = atByte `testBit` (2*quad)
            let pSel =
                    if atBitA
                    then (if atBitB then Pal3 else Pal2)
                    else (if atBitB then Pal1 else Pal0)
            let pal = selectPalette pals pSel
            let Palette { c1,c2,c3 } = pal
            let x3 = x `testBit` 3
            let y3 = y `testBit` 3
            let col =
                    if y3
                    then (if x3 then c3 else c2)
                    else (if x3 then c1 else bg)
            return col

    Screen { height = 240, width = 256, cols }


screenBG :: Palettes -> [Byte] -> PAT -> Screen
screenBG pals kb (PAT pt) = do
    let nt = listArray (0,959) (take 960 kb)
    let at = listArray (0,63) (drop 960 kb)
    let cols = do
            y <- [0..239]
            x <- [0..255]
            let nti = 32*(y`div`8) + x`div`8
            let ti = fromIntegral $ unByte $ nt ! nti
            let pti = 16*ti + y`mod`8
            let tileByteA = pt ! (pti+8)
            let tileByteB = pt ! pti
            let tileBitA = tileByteA `testBit` (7 - x`mod`8)
            let tileBitB = tileByteB `testBit` (7 - x`mod`8)
            let cSel =
                    if tileBitA
                    then (if tileBitB then CS3 else CS2)
                    else (if tileBitB then CS1 else BG)
            let ati = 8*(y`div`32) + x`div`32
            let atByte  = at ! ati
            let x4 = x `testBit` 4
            let y4 = y `testBit` 4
            let quad = if y4 then (if x4 then 3 else 2) else (if x4 then 1 else 0) -- 0..3
            let atBitA = atByte `testBit` (2*quad + 1)
            let atBitB = atByte `testBit` (2*quad)
            let pSel =
                    if atBitA
                    then (if atBitB then Pal3 else Pal2)
                    else (if atBitB then Pal1 else Pal0)
            let pal = selectPalette pals pSel
            let Palettes{bg} = pals
            return $ selectColour bg pal cSel
    Screen { height = 240, width = 256, cols }



screenSprites :: Palettes -> [Byte] -> PAT -> Screen
screenSprites pals oamBytes pat = do
    let Palettes{bg} = pals
    let sprites = seeSprites pals oamBytes pat
    let cols = do
            y <- [0..239::Int]
            x <- [0..255::Int]
            let sOpt =
                    listToMaybe $
                    flip filter sprites $ \Sprite{x=xp,y=yp} ->
                                              yp<=y && y<yp+8 && xp<=x && x<xp+8
            case sOpt of
                Nothing -> return bg
                Just Sprite{x=xp,y=yp,screen=Screen{cols}} -> do
                    let yi = y - yp
                    let xi = x - xp
                    let i = 8 * yi + xi
                    let col = cols !! i
                    return col

    Screen { height = 240, width = 256, cols }



screenCombined :: Palettes -> ([Byte],PAT) -> ([Byte],PAT) -> Screen
screenCombined pals (oamBytes,pat1) (kb,pat2) = do
    let Palettes{bg} = pals
    let nt = listArray (0,959) (take 960 kb)
    let at = listArray (0,63) (drop 960 kb)
    let sprites = seeSprites pals oamBytes pat1
    let cols = do
            y <- [0..239]
            x <- [0..255]
            let sOpt =
                    listToMaybe $
                    flip filter sprites $ \Sprite{x=xp,y=yp} ->
                                              yp<=y && y<yp+8 && xp<=x && x<xp+8

            case sOpt of
                Just Sprite{x=xp,y=yp,screen=Screen{cols}} -> do
                    let yi = y - yp
                    let xi = x - xp
                    let i = 8 * yi + xi
                    let col = cols !! i
                    return col
                Nothing -> do
                    let (PAT pt) = pat2
                    let nti = 32*(y`div`8) + x`div`8
                    let ti = fromIntegral $ unByte $ nt ! nti
                    let pti = 16*ti + y`mod`8
                    let tileByteA = pt ! (pti+8)
                    let tileByteB = pt ! pti
                    let tileBitA = tileByteA `testBit` (7 - x`mod`8)
                    let tileBitB = tileByteB `testBit` (7 - x`mod`8)
                    let cSel =
                            if tileBitA
                            then (if tileBitB then CS3 else CS2)
                            else (if tileBitB then CS1 else BG)
                    let ati = 8*(y`div`32) + x`div`32
                    let atByte  = at ! ati
                    let x4 = x `testBit` 4
                    let y4 = y `testBit` 4
                    let quad = if y4 then (if x4 then 3 else 2) else (if x4 then 1 else 0) -- 0..3
                    let atBitA = atByte `testBit` (2*quad + 1)
                    let atBitB = atByte `testBit` (2*quad)
                    let pSel =
                            if atBitA
                            then (if atBitB then Pal3 else Pal2)
                            else (if atBitB then Pal1 else Pal0)
                    let pal = selectPalette pals pSel
                    return $ selectColour bg pal cSel
    Screen { height = 240, width = 256, cols }
