
module Sim(
    --gloss,
    ) where
{-
import Text.Printf (printf)
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Graphics.Gloss(pictures,translate,scale)
import Graphics.Gloss.Interface.IO.Game as Gloss(Event(..),Key(..),SpecialKey(..),KeyState(..))

import Graphics(Screen,pictureScreen,screenTiles)
import System(State(..),state0,stepCPU,stepNMI,renderScreen)

import qualified Controller

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

        fps = 2 -- 60 -- slow for dev

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
handleEventModel event model@Model{state=state@State{con}} = do
    putStrLn "E"
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
        _ -> model
  where
        joy = \case Down -> press; Up -> release
        press but = mod $ state { con = Controller.pressJ1 but con }
        release but = mod $ state { con = Controller.releaseJ1 but con }
        mod state = model { state }


updateModel :: Float -> Model -> IO Model
updateModel _delta m@Model{i,state} = do -- called once per frame, every 1/60s
    --putStrLn $ showFI (0::Int) <> ": " <> show state
    state1 <- loop 0 state
    --putStrLn "--------------------------------------------------"
    let state' = stepNMI state1
    return m
        { i = i + 1
        , state = state'
        , screen = renderScreen state'
        }
 where
    showFI n = printf "%5d.%04d" i n -- frame/instruction

    loop :: Int -> State -> IO State
    loop n state = do
        -- instructions/frame. needs to be about 10k
        if n == 10000 then return state else do
            --let State{con} = state
            putStrLn $ showFI n <> ": " <> show state -- <> " -- " <> show con
            loop (n+1) (stepCPU state)
-}
