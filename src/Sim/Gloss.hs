
module Sim.Gloss(run) where

import Data.IORef
import qualified Data.Set as Set

import Graphics.Gloss (translate,scale,pictures,color,cyan,Picture(..))
import Graphics.Gloss.Interface.IO.Game(Event(..),Key(..),SpecialKey(..),KeyState(..))
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Graphics.Gloss.Data.Bitmap

import PPU.Render(Display(..))
import Sim.World
import qualified Controller
import qualified PPU.Graphics as Graphics

run :: String -> Bool -> Int -> IO ()
run path fs sc = do
    let debug = True
    model <- world0 path

    lastFrameCountRef <- newIORef 0

    Gloss.playIO dis (Gloss.greyN 0.3) fps model
        (\  m -> do pic <- pictureWorld lastFrameCountRef m; return $ doPosition pic)
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
handleEventWorld event world@World{buttons,paused} = do
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
        EventKey (SpecialKey KeySpace) Down _ _ -> flipPause
        _ -> world
  where
        joy = \case Down -> press; Up -> release
        press but = world { buttons = Set.insert but buttons }
        release but = world { buttons = Set.delete but buttons }
        flipPause = world { paused = not paused }

pictureWorld :: IORef Int -> World -> IO Gloss.Picture
pictureWorld lastFrameCountRef World{frameCount,display,buttons} = do
    lastFrameCount <- readIORef lastFrameCountRef
    writeIORef lastFrameCountRef frameCount
    let droppedFrames = frameCount - lastFrameCount
    return $ pictures
        [ scale 1 (-1) $ makePicture display
        , translate 0 (-320) $ scale 0.5 0.5 $ color cyan $ Text (show droppedFrames)
        , translate 0 (-380) $ scale 0.5 0.5 $ color cyan $ Text (show frameCount)
        , translate 150 (-380) $ scale 0.5 0.5 $ color cyan $ Text (Controller.showPressed buttons)
        ]

makePicture :: Display -> Gloss.Picture
makePicture Display{bg1,at1,pals} = do --,bg2,tiles1,tiles2} = do
    pictures
        [ pictureScreen bg1
        , translate 300 0 $ pictureScreen at1
--        , translate 300 0 $ pictureScreen bg2
--        , translate 600 0 $ pictureScreen tiles1
--        , translate 600 150 $ pictureScreen tiles2

        , translate 600 0 $ pictures $
          map (\(pal,i) -> translate 0 (30*i) $ pictureScreen pal) (zip pals [0..])

        ]

pictureScreen :: Graphics.Screen -> Gloss.Picture
pictureScreen screen = translate (fromIntegral w/2) (fromIntegral h/2) bitmap
    where
        bitmap = bitmapOfByteString w h format byteString False
        format = BitmapFormat { rowOrder = BottomToTop, pixelFormat = PxRGBA }
        w = Graphics.screenWidth screen
        h = Graphics.screenHeight screen
        byteString = Graphics.screenToBitmapByteString screen
