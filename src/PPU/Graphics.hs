
module PPU.Graphics(
    PAT, patFromBS,
    Screen(..),
    --screenTiles,
    screenBG,
    Palettes(..),Palette(..),
    ) where

-- CHR is a low level collectikon of bytes, found at particular offsets in the rom
-- This PAT, which is a higher level object, ready for graphical display

import Data.Array((!),listArray)
import Data.Bits(testBit)
import Data.List.Split(chunksOf)
import Data.Tuple.Extra((***))

import Byte(Byte,byteToUnsigned)
import PPU.Colour

-- TODO: kill/disable this crazy expensive checking...?
expect :: Char -> Int -> [a] -> [a]
--expect tag n xs = if n == len then xs else error $ "expect[" <> [tag] <> "](length): " <> show n <> ", got: " <> show len
--    where len = length xs
expect _ _ xs = xs

patFromBS :: [Byte] -> PAT
patFromBS = PAT . map tileFromBS . expect '3' 256 . chunksOf 16 . expect '4' 0x1000

{-screenTiles :: PAT -> Screen -- see all 256 tiles in the PAT
screenTiles =
    aboves
    . map (besides . map screenFromTile)
    . chunksOf 16
    . expect '9' 256
    . unPAT-}

{-screenFromTile :: Tile -> Screen
screenFromTile (Tile xss) = do
    let bg = black
    let pal = Palette { c1 = red, c2 = green, c3 = white }
    Screen $ map (map (selectColour bg pal)) xss
-}

screenBG :: Palettes -> [Byte] -> PAT -> Screen
screenBG palettes kilobyte pat = do
    let (nt,at) = splitAt 960 kilobyte
    let someNameTable = nameTableOfBS (expect '2' 960 nt)
    let someAttributeTable = attributeTableOfBS at
    decode pat someNameTable someAttributeTable palettes

tileFromBS :: [Byte] -> Tile
tileFromBS = \bs -> do
    let (x,y) = splitAt 8 bs
    Tile $ chunksOf 8 $ zipWith (curry makeColourSelect) (bits x) (bits y)
    where
        bits :: [Byte] -> [Bool]
        bits = concat . map explodeByte

explodeByte :: Byte -> [Bool]
explodeByte b = expect '9' 8 $ map (testBit b) (reverse [0..7])

makeColourSelect :: (Bool,Bool) -> ColourSelect
makeColourSelect = \case (False,False) -> BG; (False,True) -> CS1; (True,False) -> CS2; (True,True) -> CS3

nameTableOfBS :: [Byte] -> NameTable
nameTableOfBS = NameTable . expect '5' 30 . chunksOf 32 . expect '6' 960

attributeTableOfBS :: [Byte] -> AttributeTable
attributeTableOfBS =
    AttributeTable
    . expect 'z' 15
    . take 15
    . expect 'x' 16 . map (expect 'y' 16)
    . map (
           (\(top,bot) -> concat top <> concat bot)
           . unzip
           . expect 'h' 4
           . deleave
           . expect 'f' 8 . map (expect 'g' 2)
           . chunksOf 2
           . expect 'e' 16
          )
    . expect 'c' 16 . map (expect 'd' 16)
    . chunksOf 16
    . expect 'b' 256
    . concat
    . map (concat . makeSquareOfPaletteSelects)
    . expect 'a' 64

deleave :: [a] -> [(a,a)]
deleave = \case
    [] -> []
    [_] -> error "deleave"
    a1:a2:as -> (a1,a2) : deleave as

makeSquareOfPaletteSelects :: Byte -> [[PaletteSelect]] -- 2,2
makeSquareOfPaletteSelects =
    map (map makePaletteSelect)
    . expect '7' 2
    . map (expect '8' 2)
    . flip pick layout
    where
        pick b = map (map (testBit b *** testBit b))
        layout = [[(7,6),(5,4)],
                  [(3,2),(1,0)]]

makePaletteSelect :: (Bool,Bool) -> PaletteSelect
makePaletteSelect = \case (False,False) -> Pal1; (False,True) -> Pal2; (True,False) -> Pal3; (True,True) -> Pal4

decode :: PAT -> NameTable -> AttributeTable -> Palettes -> Screen
decode pat (NameTable nt) (AttributeTable at) palettes = screen
    where
        tiles = listArray (0,255) $ unPAT pat
        screen =
             aboves
            . map (besides . map processTile) $ ntat

        ntat :: [[(TileSelect,PaletteSelect)]]
        ntat =
            expect 'a' 30 $ map (expect 'b' 32) $
            zipWith zip nt (dubdub at)

        processTile :: (TileSelect,PaletteSelect) -> Screen -- [[Colour]]
        processTile (ts,ps) =
            (Screen
             . expect '1' 8
             . map (expect '2' 8)
             . map (map (processPixel ps))
            ) $ xss
            where (Tile xss) = getTile ts

        getTile :: TileSelect -> Tile
        getTile = (tiles !) . byteToUnsigned

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

-- screens should be rectangular, but nothing enforces that.. still, we can have some combinators

beside :: Screen -> Screen -> Screen -- chops taller screen
beside (Screen xss) (Screen yss) = Screen (zipWith (<>) xss yss)

above :: Screen -> Screen -> Screen -- allow ragged lines
above s1 s2 = Screen (unScreen s1 <> unScreen s2)

besides :: [Screen] -> Screen
besides = \case
    [] -> error "besides:[]"
    s1:screens -> foldr beside s1 screens

aboves :: [Screen] -> Screen
aboves = \case
    [] -> error "aboves:[]"
    s1:screens -> foldr above s1 screens

newtype PAT = PAT { unPAT :: [Tile] } --256

newtype Screen = Screen { unScreen :: [[Colour]] } --240,256 (full screen), but we can manage smaller frags

newtype Tile = Tile [[ColourSelect]] -- 8,8

data ColourSelect = BG | CS1 | CS2 | CS3

newtype NameTable = NameTable [[TileSelect]] -- 30,32

type TileSelect = Byte

newtype AttributeTable = AttributeTable [[PaletteSelect]] -- 15,16

data PaletteSelect = Pal1 | Pal2 | Pal3 | Pal4

data Palette = Palette { c1,c2,c3 :: Colour }

data Palettes = Palettes { p1,p2,p3,p4 :: Palette, bg :: Colour }
