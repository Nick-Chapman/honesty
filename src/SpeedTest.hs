
module SpeedTest(run) where

import Data.Time (UTCTime,getCurrentTime,diffUTCTime,nominalDiffTimeToSeconds)
import Text.Printf (printf)
import Data.Fixed

import Sim.World(world0,updateWorld)

run :: String -> IO ()
run path = do
    time0 <- getCurrentTime
    w0 <- world0 path
    testStepper time0 0 w0 time0 (updateWorld False 0)

testStepper :: UTCTime -> Int -> a -> UTCTime -> (a -> IO a) -> IO ()
testStepper time frames state time0 step = do
    state' <- step state
    time' <- getCurrentTime
    let frames' = frames + 1
    let fpsX = makeFps frames' time0 time'
    let fpsY = makeFps 1       time  time'
    putStrLn $ show frames <> (if (True) then "x" else ".")
        <> "[" <> show fpsX <> "] " <> show fpsY
    testStepper time' frames' state' time0 step

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
