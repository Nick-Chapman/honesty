
module Sim(
    gloss,
    ) where

import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Graphics.Gloss(pictures,translate,scale)
import Graphics.Gloss.Interface.IO.Game as Gloss(Event(..),Key(..),SpecialKey(..),KeyState(..))

import Graphics(Screen,screenFromCHR,pictureScreen)
import System(State(..),CHR,state0,step)
import Six502.Emu(Cpu(..))

data Model = Model
    { i :: Int
    , state :: State
    }

gloss :: String -> Bool -> Int -> IO ()
gloss path fg sc = do
    state <- state0 path
    let model0 = Model {i = 0, state}
    Gloss.playIO dis (Gloss.greyN 0.3) fps model0 -- "-" below flips the vertical !
        (\  m -> do putStrLn "."; return $ scale scF (-scF) $ pictureModel m)
        (\e m -> handleEventModel e m)
        (\d m -> updateModel d m)
    where
        dis = if fg
              then Gloss.FullScreen
              else Gloss.InWindow "NES" (sc * 288,sc * 144) (100,0)
        fps = 60
        scF = fromIntegral sc

pictureModel :: Model -> Gloss.Picture
pictureModel Model{state=State{chr1,chr2}} = do
    let screen1 :: Screen = screenFromCHR (chr1 :: CHR)
    let screen2 = screenFromCHR chr2
    pictures
        [ translate (-72) 0 $ pictureScreen screen1
        , translate   72  0 $ pictureScreen screen2
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
    putStrLn $ unwords [show i, show pc, show cc, show _delta]
    state <- loop 1 state
    return m
        { i = i + 1
        , state
        }
 where
     -- TODO: get real time & verify the fps
    loop :: Int -> State -> IO State
    loop n state = do
        --putStrLn $ show i <> "." <> show n <> ": " <> show state
        if n == 1000 -- instructions/frame. needs to be about 10k
            then return state
            else loop (n+1) (step state)
