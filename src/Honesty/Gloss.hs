
module Honesty.Gloss(Size(..),run) where

import Data.IORef
import Graphics.Gloss (translate,scale,pictures,color,cyan,Picture(..))
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game(Event(..),Key(..),SpecialKey(..),KeyState(..))
import qualified Data.Set as Set
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Honesty.PPU.Colour
import Honesty.PPU.Graphics(Sprite(..),Priority(..),Screen(..),mkBS)
import Honesty.PPU.Render(Display(..))
import Honesty.World as World
import qualified Honesty.Controller as Controller
import qualified Honesty.PPU.Graphics as Graphics

data Size = Tiny | Normal | Full

run :: String -> Size -> Int -> IO ()
run path size fps = do
    let debug = True
    model <- world0 path

    lastFrameCountRef <- newIORef 0

    Gloss.playIO dis (Gloss.greyN 0.3) fps model
        (\  m -> do pic <- pictureWorld lastFrameCountRef m; return $ doPosition pic)
        (\e m -> handleEventWorld e m)
        (\d m -> updateWorld debug d m)
    where

        (sc,full) = case size of
            Tiny -> (1,False)
            Normal -> (2,False)
            Full -> (3,True)

        dis = if full
              then Gloss.FullScreen
              else Gloss.InWindow "NES" (sc * x,sc * y) (0,0)

        doPosition = doScale . doBorder . doTransOriginUL
        doScale = scale (fromIntegral sc) (fromIntegral sc)

        doBorder = translate 10 (-10)
        doTransOriginUL = translate (- ((fromIntegral x)/2)) ( ((fromIntegral y)/2))

        x = 640
        y = 360

handleEventWorld :: Gloss.Event -> World -> IO World
handleEventWorld event world@World{buttons,paused,chooseL,chooseR,debugSprites} = do
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

        EventKey (SpecialKey KeySpace) Down _ _ -> world { paused = not paused }
        EventKey (SpecialKey KeyF1) Down _ _ -> world { chooseL = tail chooseL }
        EventKey (SpecialKey KeyF2) Down _ _ -> world { chooseR = tail chooseR }
        EventKey (SpecialKey KeyF3) Down _ _ -> world { debugSprites = not debugSprites }
        _ -> world
  where
        joy = \case Down -> press; Up -> release
        press but = world { buttons = Set.insert but buttons }
        release but = world { buttons = Set.delete but buttons }

pictureWorld :: IORef Int -> World -> IO Gloss.Picture
pictureWorld lastFrameCountRef world@World{frameCount,buttons} = do
    lastFrameCount <- readIORef lastFrameCountRef
    writeIORef lastFrameCountRef frameCount
    let droppedFrames = frameCount - lastFrameCount
    return $ pictures
        [ scale 1 (-1) $ makePicture world
        , translate 0 (-290) $ scale 0.3 0.3 $ color cyan $ Text (show droppedFrames)
        , translate 0 (-330) $ scale 0.3 0.3 $ color cyan $ Text (show frameCount)
        , translate 150 (-330) $ scale 0.3 0.3 $ color cyan $ Text (Controller.showPressed buttons)
        ]

makePicture :: World -> Gloss.Picture
makePicture World{chooseL,chooseR,debugSprites,display=display@Display{bg,sprites}} = do
    pictures
        [ choosePicture display (head chooseL)
        , translate 280 0 $ choosePicture display (head chooseR)
--        , translate 600 0 $ pictureScreen tiles1
--        , translate 600 150 $ pictureScreen tiles2
--        , translate 600 0 $ pictures $
--          map (\(pal,i) -> translate 0 (30*i) $ pictureScreen pal) (zip pals [0..])
        ,
          if debugSprites
          then
              translate 560 0 $ pictures $
              map (\(sprite,i) -> translate 0 (10*i) $ pictureSprite bg sprite) (zip sprites [0..])
          else pictures []
        ]

choosePicture :: Display -> ChooseToDisplay -> Picture
choosePicture Display{at,pf,spr,combined} = \case
    ChooseNothing -> pictures []
    ChooseAT -> pictureScreen at
    ChoosePlayfield -> pictureScreen pf
    ChooseOnlySprites -> pictureScreen spr
    ChooseCombined -> pictureScreen combined

pictureSprite :: Colour -> Graphics.Sprite -> Gloss.Picture
pictureSprite bg Sprite{ocs,x,y,priority} = do
    let cols = map (\case Just col -> col; Nothing -> bg) ocs
    pictures
        [ pictureScreen $ Screen { height = 8, width = 8, bs = mkBS cols }
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
