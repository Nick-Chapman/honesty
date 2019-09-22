
module Sim(
    gloss,
    ) where

import Text.Printf (printf)
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Graphics.Gloss(pictures,translate,scale)
import Graphics.Gloss.Interface.IO.Game as Gloss(Event(..),Key(..),SpecialKey(..),KeyState(..))

import Graphics(Screen,pictureScreen,screenTiles)
import System(State(..),state0,stepCPU,renderScreen)
import Six502.Emu(Cpu(..))

data Model = Model
    { i :: Int
    , state :: State
    , screen :: Screen
    }

gloss :: String -> Bool -> Int -> IO ()
gloss path fg sc = do
    state <- state0 path
    let screen = renderScreen state
    let model0 = Model {i = 0, state, screen}
    Gloss.playIO dis (Gloss.greyN 0.3) fps model0
        (\  m -> do --putStrLn ".";
                    return $ doPosition $ pictureModel m)
        (\e m -> handleEventModel e m)
        (\d m -> updateModel d m)
    where
        dis = if fg
              then Gloss.FullScreen
              else Gloss.InWindow "NES" (sc * x,sc * y) (0,0)

        fps = 2 --60 -- slow for dev

        doPosition = doScale . doBorder . doTransOriginUL
        doScale = scale scF (-scF) -- The "-" flips the vertical
        scF = fromIntegral sc

        doBorder = translate 10 10
        doTransOriginUL = translate (- ((fromIntegral x)/2)) (- ((fromIntegral y)/2))

        x = 800
        y = 600


pictureModel :: Model -> Gloss.Picture
pictureModel Model{state=State{chr1,chr2},screen} = do
    let left = screenTiles chr1
    let right = screenTiles chr2
    pictures
        [ pictureScreen screen
        , translate 300 0 $ pictureScreen left
        , translate 500 0 $ pictureScreen right
        ]

handleEventModel :: Gloss.Event -> Model -> IO Model
handleEventModel event model = do
    putStrLn "E"
    case event of
        EventKey (SpecialKey KeyEsc) Down _ _ -> error "quit"
        _ -> return model

updateModel :: Float -> Model -> IO Model
updateModel _delta m@Model{i,state} = do -- called once per frame, every 1/60s
    let State{cpu=Cpu{pc},cc} = state
    let _ = (cc,pc)
    state <- loop 0 state
    --putStrLn $ showFI (0::Int) <> ": " <> show state
    --putStrLn "--------------------------------------------------"
    let screen = renderScreen state
    return m
        { i = i + 1
        , state
        , screen
        }
 where
    showFI n = printf "%5d.%02d" i n -- frame/instruction

    loop :: Int -> State -> IO State
    loop n state = do
        -- instructions/frame. needs to be about 10k
        if n == 1000 then return state else do
            putStrLn $ showFI n <> ": " <> show state
            loop (n+1) (stepCPU state)
