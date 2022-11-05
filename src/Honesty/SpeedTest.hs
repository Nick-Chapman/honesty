
module Honesty.SpeedTest(run) where

import Data.Fixed (Fixed(..),HasResolution,resolution)
import Data.Time (UTCTime,getCurrentTime,diffUTCTime,nominalDiffTimeToSeconds)
import Honesty.PPU.Graphics (screenToBitmapByteString)
import Honesty.PPU.Render (Display(..))
import Honesty.World (World(..),world0,updateWorld)
import Text.Printf (printf)
import qualified Data.ByteString as BS (foldl')

run :: String -> Maybe Int -> IO ()
run path maxM = do
    time0 <- getCurrentTime
    w0 <- world0 path False
    let quit :: Int -> Bool = case maxM of Nothing -> const False; Just n -> (>n)
    testStepper time0 0 w0 time0 updateWorld forceWorld quit

forceWorld :: World -> Int
forceWorld World{display=Display{combined}} = do
    let bs = screenToBitmapByteString combined
    fromIntegral $ BS.foldl' (+) 0 bs

testStepper :: UTCTime -> Int -> a -> UTCTime -> (a -> IO a) -> (a -> Int) -> (Int -> Bool) -> IO ()
testStepper time frames state time0 step force quit = if quit frames then return () else do
    state' <- step state
    let forced = force state'
    time' <- getCurrentTime
    let frames' = frames + 1
    let fpsX = makeFps frames' time0 time'
    let fpsY = makeFps 1       time  time'
    putStrLn $ show frames <> " (" <> printf "%03d" forced <> ") "
        <> "[" <> show fpsX <> "] " <> show fpsY
    testStepper time' frames' state' time0 step force quit

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
