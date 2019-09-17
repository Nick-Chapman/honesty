
-- First steps WIP for Nes/PPU/graphics emulation...

module Nes(
    CHR,
    glossMainShowChr,
    chrFromBS
    ) where

import Data.Bits (shiftR, (.&.), testBit)
import Data.List.Split(chunksOf)
import Data.List (splitAt)
import Data.Tuple.Extra( (***) )
import Data.Word (Word32)
import Data.Word8 (Word8)

import qualified Data.ByteString as BS(ByteString,pack)
import qualified Graphics.Gloss as Gloss

import Six502.Values

glossMainShowChr :: CHR -> IO ()
glossMainShowChr chr =
    Gloss.display dis (Gloss.greyN 0.5) (pictureCHR chr)
    where dis = Gloss.InWindow "NES" (200,200) (300,300)

pictureCHR :: CHR -> Gloss.Picture
pictureCHR chr = do
    let kilobyte = expect '1' 1024 $ take 1024 (cycle [0x0..])
    let (nt,at) = splitAt 960 kilobyte
    let someNameTable = nameTableOfBS (expect 'a' 960 nt)
    let someAttributeTable = attributeTableOfBS at
    let screen = decode chr someNameTable someAttributeTable somePalettes
    glossScreen screen
        where cycle xs = xss where xss = xs <> xss

glossScreen :: Screen -> Gloss.Picture
glossScreen screen =
    Gloss.bitmapOfByteString 256 240
    (Gloss.BitmapFormat Gloss.TopToBottom Gloss.PxRGBA)
    (screenToByteString screen)
    False

screenToByteString :: Screen -> BS.ByteString
screenToByteString (Screen xss) = BS.pack $ concat $ map (chop . colourToRGBA) $ concat xss
    where chop :: Word32 -> [Word8]
          chop w =
              [ fromIntegral (shiftR w 24  .&. 0xff)
              , fromIntegral (shiftR w 16  .&. 0xff)
              , fromIntegral (shiftR w 8   .&. 0xff)
              , fromIntegral (       w     .&. 0xff)
              ]

colourToRGBA :: Colour -> Word32
colourToRGBA = \case
    Black -> 0x00000000
    White -> 0xffffff00
    Red ->   0xff000000
    Green -> 0x00ff0000

somePalettes :: Palettes
somePalettes = Palettes { p1=pal,p2=pal,p3=pal,p4=pal, bg = Black }
    where pal = Palette { c1 = Red, c2 = Green, c3 = White }

chrFromBS :: [Byte] -> CHR
chrFromBS = CHR . map tileFromBS . expect 'b' 256 . chunksOf 16 . expect 'c' 0x1000

tileFromBS :: [Byte] -> Tile
tileFromBS = \bs -> do
    let (x,y) = splitAt 8 bs
    Tile $ chunksOf 8 $ zipWith (curry makeColourSelect) (bits x) (bits y)
    where
        bits :: [Byte] -> [Bool]
        bits = concat . map explodeByte

makeColourSelect :: (Bool,Bool) -> ColourSelect
makeColourSelect = \case (False,False) -> BG; (False,True) -> CS1; (True,False) -> CS2; (True,True) -> CS3

nameTableOfBS :: [Byte] -> NameTable
nameTableOfBS = NameTable . expect 'd' 30 . chunksOf 32 . expect 'e' 960

attributeTableOfBS :: [Byte] -> AttributeTable -- TODO: this is wrong!!
attributeTableOfBS =
    AttributeTable
    . expect 'f' 15 . map (expect 'g' 16)
    . take 15
    . expect 'h' 16
    . concat
    . map (expect 'i' 2 . map (expect 'j' 16)
           . map (concat . makeSquareOfPaletteSelects)
           . expect 'o' 8
          )
    . expect 'p' 8
    . chunksOf 8
    . expect 'k' 64

makeSquareOfPaletteSelects :: Byte -> [[PaletteSelect]] -- 2,2
makeSquareOfPaletteSelects =
    map (map makePaletteSelect)
    . expect 'l' 2
    . map (expect 'm' 2)
    . flip pick layout
    where
        pick b = map (map (testBit b *** testBit b))
        layout = [[(7,6),(5,4)],
                  [(3,2),(1,0)]]

explodeByte :: Byte -> [Bool]
explodeByte b = expect 'n' 8 $ map (testBit b) [7..]

makePaletteSelect :: (Bool,Bool) -> PaletteSelect
makePaletteSelect = \case (False,False) -> Pal1; (False,True) -> Pal2; (True,False) -> Pal3; (True,True) -> Pal4


expect :: Char -> Int -> [a] -> [a]
expect tag n xs = if n == len then xs else error $ "expect[" <> [tag] <> "](length): " <> show n <> ", got: " <> show len
    where len = length xs

decode :: CHR -> NameTable -> AttributeTable -> Palettes -> Screen
decode (CHR tiles) (NameTable nt) (AttributeTable at) palettes = screen
    where
        screen = Screen . map concat . concat . map (map processTile) $ ntat

        ntat :: [[(TileSelect,PaletteSelect)]]
        ntat = zipWith zip nt (dubdub at)

        processTile :: (TileSelect,PaletteSelect) -> [[Colour]]
        processTile (ts,ps) = map (map (processPixel ps)) xss
            where (Tile xss) = getTile ts

        getTile :: TileSelect -> Tile
        getTile = (tiles !!) . byteToUnsigned

        processPixel :: PaletteSelect -> ColourSelect -> Colour
        processPixel ps cs = col
            where
                col = selectColour bg pal cs
                pal = selectPalette palettes ps
                Palettes{bg} = palettes

dubdub :: [[a]] -> [[a]]
dubdub = dub . map dub
    where
        dub :: [a] -> [a]
        dub = concat . map (\x -> [x,x])

selectColour :: Colour -> Palette -> ColourSelect -> Colour
selectColour bg Palette{c1,c2,c3} = \case
    BG -> bg
    CS1 -> c1
    CS2 -> c2
    CS3 -> c3

selectPalette :: Palettes -> PaletteSelect -> Palette
selectPalette Palettes{p1,p2,p3,p4} = \case
    Pal1 -> p1
    Pal2 -> p2
    Pal3 -> p3
    Pal4 -> p4


newtype Screen = Screen [[Colour]] --240,256

newtype CHR = CHR [Tile] --256

newtype Tile = Tile [[ColourSelect]] -- 8,8

data ColourSelect = BG | CS1 | CS2 | CS3

newtype NameTable = NameTable [[TileSelect]] -- 30,32

type TileSelect = Byte

newtype AttributeTable = AttributeTable [[PaletteSelect]] -- 15,16

data PaletteSelect = Pal1 | Pal2 | Pal3 | Pal4

data Palette = Palette { c1,c2,c3 :: Colour }

data Palettes = Palettes { p1,p2,p3,p4 :: Palette, bg :: Colour }

data Colour = Black | White | Red | Green -- | Blue
