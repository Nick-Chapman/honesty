
module Sim.Gloss(run) where

import qualified Data.Set as Set

import Graphics.Gloss (translate,scale,pictures,color,cyan,Picture(..))
import Graphics.Gloss.Interface.IO.Game(Event(..),Key(..),SpecialKey(..),KeyState(..))
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import qualified Controller
import Sim.World
import PPU.Colour(Colour)
import qualified PPU.Colour
import qualified PPU.Graphics

run :: String -> Bool -> Int -> IO ()
run path fs sc = do
    let debug = True
    model <- world0 path
    Gloss.playIO dis (Gloss.greyN 0.3) fps model
        (\  m -> return $ doPosition (pictureWorld m))
        (\e m -> handleEventWorld e m)
        (\d m -> updateWorld debug d m)
    where
        dis = if fs
              then Gloss.FullScreen
              else Gloss.InWindow "NES" (sc * x,sc * y) (0,0)

        fps = 20 -- 60

        doPosition = doScale . doBorder . doTransOriginUL
        doScale = scale (fromIntegral sc) (fromIntegral sc)

        doBorder = translate 10 (-10)
        doTransOriginUL = translate (- ((fromIntegral x)/2)) ( ((fromIntegral y)/2))

        x = 800
        y = 400

handleEventWorld :: Gloss.Event -> World -> IO World
handleEventWorld event world@World{buttons} = do
    --print event
    return $ case event of
        EventKey (SpecialKey KeyEsc) Down _ _ -> error "quit"
        EventKey (Char 'z') ud _ _ -> joy ud Controller.A
        EventKey (Char 'x') ud _ _ -> joy ud Controller.B
        EventKey (SpecialKey KeyTab) ud _ _ -> joy ud Controller.Select
        EventKey (SpecialKey KeyEnter) ud _ _ -> joy ud Controller.Start
        EventKey (SpecialKey KeyUp) ud _ _ -> joy ud Controller.Up
        EventKey (SpecialKey KeyDown) ud _ _ -> joy ud Controller.Down
        EventKey (SpecialKey KeyLeft) ud _ _ -> joy ud Controller.Left
        EventKey (SpecialKey KeyRight) ud _ _ -> joy ud Controller.Right
        _ -> world
  where
        joy = \case Down -> press; Up -> release
        press but = world { buttons = Set.insert but buttons }
        release but = world { buttons = Set.delete but buttons }


pictureWorld :: World -> Gloss.Picture
pictureWorld World{frameCount,rr,display,buttons} = pictures
    [ scale 1 (-1) $ makePicture rr display
    , translate 0 (-380) $ scale 0.5 0.5 $ color cyan $ Text (show frameCount)
    , translate 150 (-380) $ scale 0.5 0.5 $ color cyan $ Text (Controller.showPressed buttons)
    ]


makePicture :: NesRamRom -> Display -> Gloss.Picture
--makePicture NesRamRom{pat1,pat2} Display{bg1,bg2} = do
makePicture _ Display{bg1} = do
    --let _ = screenTiles
    --let left = screenTiles pat1
    --let right = screenTiles pat2
    pictures
        [ pictureScreen bg1
        --, translate 300 0 $ pictureScreen bg2
        --, translate 600 0 $ pictureScreen left
        --, translate 600 150 $ pictureScreen right
        ]


pictureScreen :: PPU.Graphics.Screen -> Gloss.Picture
pictureScreen (PPU.Graphics.Screen grid) =
    Gloss.pictures $ zipWith doLine [0..] grid
    where
        doLine y scan = Gloss.pictures $ zipWith (doPixel y) [0..] scan
        doPixel y x c = point x y c

point :: Int -> Int -> Colour -> Gloss.Picture
point x y c = Gloss.color (toGlossColor c) (pixel (fromIntegral x,fromIntegral y))

toGlossColor :: Colour -> Gloss.Color
toGlossColor col = do
    let (r,g,b) = PPU.Colour.toRGB col
    Gloss.makeColorI r g b 255

pixel :: Gloss.Point -> Gloss.Picture
pixel (x,y) = Gloss.polygon [(x,y),(x,y+1),(x+1,y+1),(x+1,y)]

