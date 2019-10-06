
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
import PPU.Graphics(Sprite(..),Priority(..))

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

        x = 640
        y = 360

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
        , translate 0 (-290) $ scale 0.3 0.3 $ color cyan $ Text (show droppedFrames)
        , translate 0 (-330) $ scale 0.3 0.3 $ color cyan $ Text (show frameCount)
        , translate 150 (-330) $ scale 0.3 0.3 $ color cyan $ Text (Controller.showPressed buttons)
        ]

makePicture :: Display -> Gloss.Picture
makePicture Display{tiles1=_,tiles2=_,bg1,bg2,at1,at2,sprites} = do
    let pick = True
    let bg = if pick then bg1 else bg2
    let at = if pick then at1 else at2
    pictures
        [ pictureScreen bg
        , translate 280 0 $ pictureScreen at
--        , translate 600 0 $ pictureScreen tiles1
--        , translate 600 150 $ pictureScreen tiles2
--        , translate 600 0 $ pictures $
--          map (\(pal,i) -> translate 0 (30*i) $ pictureScreen pal) (zip pals [0..])
        , translate 560 0 $ pictures $
          map (\(sprite,i) -> translate 0 (10*i) $ pictureSprite sprite) (zip sprites [0..])
        ]

pictureSprite :: Graphics.Sprite -> Gloss.Picture
pictureSprite Sprite{screen,x,y,priority} =
    pictures
    [ pictureScreen screen
    , translate 20 5 $ scale 0.05 (-0.05) $ color cyan
      $ Text (show x <> ", " <> show y <> case priority of InFront -> ", f"; Behind -> ", b")
    ]

pictureScreen :: Graphics.Screen -> Gloss.Picture
pictureScreen screen = translate (fromIntegral w/2) (fromIntegral h/2) bitmap
    where
        bitmap = bitmapOfByteString w h format byteString False
        format = BitmapFormat { rowOrder = BottomToTop, pixelFormat = PxRGBA }
        w = Graphics.screenWidth screen
        h = Graphics.screenHeight screen
        byteString = Graphics.screenToBitmapByteString screen
