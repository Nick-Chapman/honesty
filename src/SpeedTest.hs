
module SpeedTest(run) where

import Data.Time (UTCTime,getCurrentTime,diffUTCTime,nominalDiffTimeToSeconds)
import Text.Printf (printf)
import Data.Fixed

import Top(Model(Model,display),model0,updateModel,collapseDisplay)

type World = Model

run :: String -> IO ()
run path = do
    time0 <- getCurrentTime
    w0 <- world0 path
    testStepper time0 0 w0 time0 stepWorld collapseWorld

world0 :: String -> IO World
world0 = Top.model0

stepWorld :: World -> IO World
stepWorld = Top.updateModel 0

collapseWorld :: World -> Bool
collapseWorld Model{display} = Top.collapseDisplay display

testStepper :: UTCTime -> Int -> a -> UTCTime -> (a -> IO a) -> (a -> Bool) -> IO ()
testStepper time frames state time0 step collapse = do
    state' <- step state
    time' <- getCurrentTime
    let frames' = frames + 1
    let fpsX = makeFps frames' time0 time'
    let fpsY = makeFps 1       time  time'
    putStrLn $ show frames <> (if (collapse state) then "x" else ".")
        <> "[" <> show fpsX <> "] " <> show fpsY
    testStepper time' frames' state' time0 step collapse

newtype Fps = Fps Float

instance Show Fps where show (Fps f) = printf "%.02g" f

makeFps :: Int -> UTCTime -> UTCTime -> Fps
makeFps frames a b = do
    let nd = diffUTCTime b a
    let f = fromFixed $ nominalDiffTimeToSeconds nd
    Fps (fromIntegral frames / f)

-- | Convert From Fixed to Fractional
fromFixed :: (Fractional a, HasResolution b) => Fixed b -> a
fromFixed fv@(MkFixed v) = (fromIntegral v) / (fromIntegral $ resolution fv)
