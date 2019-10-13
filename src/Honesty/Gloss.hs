
module Honesty.Gloss(Size(..),run) where

import Data.IORef
import Graphics.Gloss (translate,scale,pictures,color,cyan,blue,Picture(..))
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game(Event(..),Key(..),SpecialKey(..),KeyState(..))
import qualified Data.Set as Set
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Honesty.PPU.Colour
import Honesty.PPU.Regs as Regs(State(State,scroll_x,scroll_y),Control(..),Mask(..))
import Honesty.PPU.Graphics(Sprite(..),Priority(..),Screen(..),mkBS)
import Honesty.PPU.Render(Display(..))
import Honesty.World as World
import qualified Honesty.Controller as Controller
import qualified Honesty.PPU.Graphics as Graphics

data Size = Tiny | Normal | Full

run :: String -> Size -> Int -> Bool -> IO ()
run path size fps trace = do
    model <- world0 path trace

    lastFrameCountRef <- newIORef 0

    Gloss.playIO dis (Gloss.greyN 0.3) fps model
        (\  m -> do pic <- pictureWorld lastFrameCountRef m; return $ doPosition pic)
        (\e m -> handleEventWorld e m)
        (\_ m -> updateWorld m)
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
handleEventWorld event world@World{buttons,paused,chooseL,chooseR,
                                   debugSprites,debugFrames,debugButtons,debugRegs} = do
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
        EventKey (SpecialKey KeyF4) Down _ _ -> world { debugFrames = not debugFrames }
        EventKey (SpecialKey KeyF5) Down _ _ -> world { debugButtons = not debugButtons }
        EventKey (SpecialKey KeyF6) Down _ _ -> world { debugRegs = not debugRegs }
        _ -> world
  where
        joy = \case Down -> press; Up -> release
        press but = world { buttons = Set.insert but buttons }
        release but = world { buttons = Set.delete but buttons }

pictureWorld :: IORef Int -> World -> IO Gloss.Picture
pictureWorld lastFrameCountRef World{frameCount,fps,buttons,
                                     display=display@Display{bg,sprites,control,mask,regs},
                                     chooseL,chooseR,
                                     debugSprites=ds,
                                     debugFrames=df,
                                     debugButtons=db,
                                     debugRegs=dr
                                    } = do
    lastFrameCount <- readIORef lastFrameCountRef
    writeIORef lastFrameCountRef frameCount
    let droppedFrames = frameCount - lastFrameCount
    let Regs.State{scroll_x,scroll_y} = regs
    return $ color cyan $ pictures $
        [ scale 1 (-1) $ pictures
            [ choosePicture display (head chooseL)
            , translate 280 0 $ choosePicture display (head chooseR)
            , showIf ds $ translate 560 0 $ picSprites bg sprites
            ]
        , showIf df $ translate 0 (-290) $ scale 0.1 0.1 $ picLabelled "frame" frameCount
        , showIf df $ translate 0 (-310) $ scale 0.1 0.1 $ picLabelled "drop" droppedFrames
        , showIf df $ translate 0 (-330) $ scale 0.1 0.1 $ picLabelled "fps" fps
        , showIf db $ translate 150 (-330) $ scale 0.3 0.3 $ Text (Controller.showPressed buttons)

        , showIf dr $ translate 500 (-290) $ scale 0.1 0.1 $ picLabelled "scroll" (scroll_x,scroll_y)
        , showIf dr $ translate 500 (-310) $ scale 0.1 0.1 $ picControl control
        , showIf dr $ translate 500 (-330) $ scale 0.1 0.1 $ picMask mask
        ]

picLabelled :: Show a => String -> a -> Picture
picLabelled tag v =
    pictures [Text tag, translate 500 0 $ Text (show v) ]

choosePicture :: Display -> ChooseToDisplay -> Picture
choosePicture Display{pf,spr,combined} = \case
    ChooseNothing -> pictures []
    --ChooseAT1 -> pictureScreenTag "attributes" at1
    --ChooseAT2 -> pictureScreenTag "attributes(2)" at2
    ChoosePlayfield -> pictureScreenTag "playfield" pf
    --ChoosePlayfield2 -> pictureScreenTag "playfield(2)" pf2
    ChooseOnlySprites -> pictureScreenTag "sprites" spr
    ChooseFullGame -> pictureScreenTag "game" combined

showIf :: Bool -> Picture -> Picture
showIf b p = if b then p else pictures []

picSprites :: Colour -> [Sprite] -> Picture
picSprites bg sprites =
    pictures $ map (\(sprite,i) -> translate 0 (10*i) $ picSprite bg sprite) (zip sprites [0..])

picSprite :: Colour -> Graphics.Sprite -> Gloss.Picture
picSprite bg Sprite{ocs,x,y,priority} = do
    let cols = map (\case Just col -> col; Nothing -> bg) ocs
    pictures
        [ pictureScreen $ Screen { height = 8, width = 8, bs = mkBS cols }
        , translate 20 5 $ scale 0.05 (-0.05) $ color cyan
          $ Text (show x <> ", " <> show y <> case priority of InFront -> ", f"; Behind -> ", b")
        ]

picControl :: Control -> Picture
picControl Control
    { nmiEnabled=v
    , masterSlave=p
    , spriteHeight=h
    , backgroundTileSelect=b
    , spriteTileSelect=s
    , incrementMode=i
    , nameTableSelect1=n1
    , nameTableSelect0=n0
    } = do
    picFlags "VPHBSINN" [v,p,h,b,s,i,b,n1,n0]

picMask :: Mask -> Picture
picMask Mask
    { blueEmphasis=be
    , greenEmphasis=ge
    , redEmphasis=re
    , spriteEnable=s
    , backgroundEnable=b
    , spriteLeftColumnEnable=um
    , backgroundLeftColumnEnable=m
    , greyScale=g } =
    picFlags "BGRsbMmG" [be,ge,re,s,b,um,m,g]

picFlags :: String -> [Bool] -> Picture
picFlags flagChars flagBools =
    pictures $ map (\(i,(c,b)) ->
                            translate (120*i) 0 $ color (if b then cyan else blue) $ Text [c]) $
        zip [0..] (zip flagChars flagBools)

pictureScreenTag :: String -> Graphics.Screen -> Gloss.Picture
pictureScreenTag tag screen = pictures
    [ pictureScreen screen
    , translate 0 (fromIntegral h + 15) $ scale 0.1 (-0.1) $ Text tag
    ]
    where
        h = Graphics.screenHeight screen

pictureScreen :: Graphics.Screen -> Gloss.Picture
pictureScreen screen = translate (fromIntegral w/2) (fromIntegral h/2) bitmap
    where
        bitmap = bitmapOfByteString w h format byteString False
        format = BitmapFormat { rowOrder = BottomToTop, pixelFormat = PxRGBA }
        w = Graphics.screenWidth screen
        h = Graphics.screenHeight screen
        byteString = Graphics.screenToBitmapByteString screen
