
module Honesty.PPU.Graphics(
    PAT, patFromBS,
    Screen(..), mkBS,
    screenToBitmapByteString,screenWidth,screenHeight,
    screenTiles,
    Palettes(..),Palette(..),
    screenAT,
    screenPalettes,
    Sprite(..), Priority(..), seeSprites,
    screenCombined,
    ) where

import Data.Array
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB

import Honesty.Byte
import Honesty.PPU.Colour as Colour

newtype PAT = PAT (Array Int Byte)

patFromBS :: [Byte] -> PAT
patFromBS = PAT . listArray (0,4095)

mkBS :: [Colour] -> BS.ByteString
mkBS cols = do
    let ws = map Colour.toRGB cols
    let builder = mconcat (map BSB.word32BE ws)
    BSL.toStrict (BSB.toLazyByteString builder)

data Screen = Screen {height,width :: Int, bs :: BS.ByteString }

screenToBitmapByteString :: Screen -> BS.ByteString
screenToBitmapByteString Screen{bs} = bs

screenWidth :: Screen -> Int
screenWidth Screen{width} = width

screenHeight :: Screen -> Int
screenHeight Screen{height} = height

data ColourSelect = BG | CS1 | CS2 | CS3
data PaletteSelect = Pal0 | Pal1 | Pal2 | Pal3 | Pal4 | Pal5 | Pal6 | Pal7
data Palette = Palette { c1,c2,c3 :: Colour }
data Palettes = Palettes { p0,p1,p2,p3,p4,p5,p6,p7 :: Palette }

selectColour :: Colour -> Palette -> ColourSelect -> Colour
selectColour bg Palette{c1,c2,c3} = \case
    BG -> bg
    CS1 -> c1
    CS2 -> c2
    CS3 -> c3

selectOptColour :: Palette -> ColourSelect -> Maybe Colour
selectOptColour Palette{c1,c2,c3} = \case
    BG -> Nothing
    CS1 -> Just c1
    CS2 -> Just c2
    CS3 -> Just c3

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
data Sprite = Sprite { ocs :: [Maybe Colour], x,y::Int, priority :: Priority }

seeSprites :: Palettes -> [Byte] -> PAT -> [Sprite]
seeSprites pals oamBytes pat = do
    let oam = listArray (0,255) oamBytes
    i <- [0..63::Int]
    let sprite@Sprite{y} = seeSprite pals (oam!(4*i), oam!(4*i+1), oam!(4*i+2), oam!(4*i+3)) pat
    if y < 0xEF then [sprite] else []

seeSprite :: Palettes -> (Byte,Byte,Byte,Byte) -> PAT -> Sprite
seeSprite pals (b0,b1,b2,b3) (PAT pt) = do
    let y = byteToUnsigned b0 + 1 -- sprite data is delayed by one scanline
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
    let ocs = do
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
            return $ selectOptColour pal cSel

    Sprite { ocs, x, y, priority }


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
    Screen { height = 128, width = 128, bs = mkBS cols}


screenPalettes :: Colour -> Palettes -> [Screen]
screenPalettes bg (Palettes {p0,p1,p2,p3}) = map (screenPalette bg) [p0,p1,p2,p3]

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
    Screen { height = 16, width = 16, bs = mkBS cols }


screenAT :: Colour ->  Palettes -> [Byte] -> Screen
screenAT bg pals atBytes = do
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

    Screen { height = 240, width = 256, bs = mkBS cols }

screenCombined :: (Int,Int) -> Colour -> Palettes -> (Bool,[Byte],PAT) -> (Bool,([Byte],[Byte]),PAT) -> Screen
screenCombined (_scroll_x,scroll_y) bg pals (spriteEnable,oamBytes,patS) (backgroundEnable,(kb1,kb2),patB) = do

    let nt1 = listArray (0,959) (take 960 kb1)
    let at1 = listArray (0,63) (drop 960 kb1)

    let nt2 = listArray (0,959) (take 960 kb2)
    let at2 = listArray (0,63) (drop 960 kb2)

    let sprites = if spriteEnable then seeSprites pals oamBytes patS else []
    let bs = mkBS $ do
            y <- [0..239]

            let y1 = y + scroll_y
            let yBG = y1 `mod` 240
            let nextNT = (y1 `div` 240) `mod` 2 == 1
            let (at,nt) = if nextNT then (at2,nt2) else (at1,nt1)

            let spritesOnLine = take 8 $ flip filter sprites $ \Sprite{y=yp} -> yp<=y && y<yp+8
            x <- [0..255]
            let spritesAtPoint = flip filter spritesOnLine $ \Sprite{x=xp} -> xp<=x && x<xp+8
            let oc = case spritesAtPoint of
                         [] -> Nothing
                         Sprite{x=xp,y=yp,ocs}:_ -> do -- TODO: sprite transparency
                             let yi = y - yp
                             let xi = x - xp
                             let i = 8 * yi + xi
                             ocs !! i
            case oc of
                Just col -> [col]
                Nothing ->
                    if backgroundEnable
                    then [screenPlayField bg nt at pals patB x yBG]
                    else [bg]
    Screen { height = 240, width = 256, bs }


screenPlayField ::
    Colour
    -> Array Int Byte
    -> Array Int Byte
    -> Palettes
    -> PAT
    -> Int
    -> Int
    -> Colour
screenPlayField bg nt at pals patB x y = do
    let (PAT pt) = patB
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
    selectColour bg pal cSel
