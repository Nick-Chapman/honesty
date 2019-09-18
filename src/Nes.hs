
-- First steps WIP for Nes/PPU/graphics emulation...

module Nes(
    CHR,
    glossMainShowChr,
    chrFromBS
    ) where

import Data.Bits (testBit) -- (shiftR, (.&.), testBit)
import Data.List.Split(chunksOf)
import Data.Tuple.Extra( (***) )
--import System.IO (hFlush,stdout)

import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Graphics.Gloss(Point,Picture,translate,scale,color)

import Six502.Values

{-_glossMainShowChr :: CHR -> IO ()
_glossMainShowChr chr =
    Gloss.display dis (Gloss.yellow) (pictureCHR chr)
    where dis = Gloss.InWindow "NES" (600,600) (100,100)
-}

data Model = Model { i :: Int }

glossMainShowChr :: CHR -> IO ()
glossMainShowChr chr = do
    Gloss.playIO dis (Gloss.greyN 0.5) fps model0
        (\  m -> pictureModel chr m)
        (\e m -> handleEventModel e m)
        (\d m -> updateModel d m)
    where
        dis = Gloss.InWindow "NES" (800,600) (5000,0)
        model0 = Model {i=0}
        fps = 10

{-put :: String -> IO ()
put s = do
    do putStr s; hFlush(stdout)
    return ()
-}

pictureModel :: CHR -> Model -> IO Gloss.Picture
pictureModel chr Model{i} = do
    let screen = screenFromCHR i chr
    --put "."
    return $ translate (-200) (-200) $ scale sc sc $ pictureScreen screen
    where
        sc = 2

handleEventModel :: Gloss.Event -> Model -> IO Model
handleEventModel _event m = do
    --put "E"
    return m

updateModel :: Float -> Model -> IO Model
updateModel _delta m@Model{i} = do
    --put "U"
    return m { i = i + 1 }

pictureScreen :: Screen -> Gloss.Picture
pictureScreen (Screen grid) =
    Gloss.pictures $ zipWith doLine [0..] grid
    where
        doLine y scan = Gloss.pictures $ zipWith (doPixel y) [0..] scan
        doPixel y x c = point x y c

point :: Int -> Int -> Colour -> Gloss.Picture
point x y c = color (colourToGloss c) (pixel (fromIntegral x,fromIntegral y))

pixel :: Point -> Picture
pixel (x,y) = Gloss.polygon [(x,y),(x,y+1),(x+1,y+1),(x+1,y)]

colourToGloss :: Colour -> Gloss.Color
colourToGloss = \case
    Black -> Gloss.black
    White -> Gloss.white
    Red ->   Gloss.red
    Green -> Gloss.green


screenFromCHR :: Int -> CHR -> Screen -- just see the CHR
screenFromCHR _ =
    aboves
    . map (besides . map screenFromTile)
    . chunksOf 16
    . expect '9' 256
    . unCHR

screenFromTile :: Tile -> Screen
screenFromTile (Tile xss) = do
    let bg = Black
    let pal = Palette { c1 = Red, c2 = Green, c3 = White }
    Screen $ map (map (selectColour bg pal)) xss

_screenFromCHR :: Int -> CHR -> Screen -- see the CHR in th econtext on an invented NT/AT
_screenFromCHR i chr = do
    let kilobyte = expect '1' 1024 $ take 1024 (cycle [fromIntegral i..])
    let (nt,at) = splitAt 960 kilobyte
    let someNameTable = nameTableOfBS (expect '2' 960 nt)
    let someAttributeTable = attributeTableOfBS at
    let screen = decode chr someNameTable someAttributeTable somePalettes
    screen
        where cycle xs = xss where xss = xs <> xss


somePalettes :: Palettes
somePalettes = Palettes { p1=pal,p2=pal,p3=pal,p4=pal, bg = Black }
    where pal = Palette { c1 = Red, c2 = Green, c3 = White }


chrFromBS :: [Byte] -> CHR
chrFromBS = CHR . map tileFromBS . expect '3' 256 . chunksOf 16 . expect '4' 0x1000

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


expect :: Char -> Int -> [a] -> [a]
expect tag n xs = if n == len then xs else error $ "expect[" <> [tag] <> "](length): " <> show n <> ", got: " <> show len
    where len = length xs

decode :: CHR -> NameTable -> AttributeTable -> Palettes -> Screen
decode chr (NameTable nt) (AttributeTable at) palettes = screen
    where
        tiles = unCHR chr
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


-- screens should be square, but nothing enforces that.. still, we can have some combinators

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


newtype Screen = Screen { unScreen :: [[Colour]] } --240,256 (full screen), but we can manage smaller frags

newtype CHR = CHR { unCHR :: [Tile] } --256

newtype Tile = Tile [[ColourSelect]] -- 8,8

data ColourSelect = BG | CS1 | CS2 | CS3

newtype NameTable = NameTable [[TileSelect]] -- 30,32

type TileSelect = Byte

newtype AttributeTable = AttributeTable [[PaletteSelect]] -- 15,16

data PaletteSelect = Pal1 | Pal2 | Pal3 | Pal4

data Palette = Palette { c1,c2,c3 :: Colour }

data Palettes = Palettes { p1,p2,p3,p4 :: Palette, bg :: Colour }

data Colour = Black | White | Red | Green -- | Blue
