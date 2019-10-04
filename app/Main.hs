
module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs)

import Addr
import NesFile
import Six502.Decode (decode,reEncode)
import Six502.Disassembler (displayOpLines)
import Six502.Operations (Op)
import qualified PRG
import qualified Sim.Gloss(run)
import qualified Sim.TraceCpu(printRun)
import qualified SpeedTest(run)

main :: IO ()
main = do
    getArgs >>= \case
        ["--speed"] -> speed path
        ["--speed",path] -> speed path

        ["--dis"] -> dis path
        ["--dis",path] -> dis path

        -- Just see CPU states
        ["--emu"] -> emu path
        ["--emu",path] -> emu path

        -- NES emulation, using Gloss
        [] -> nes path
        [path] -> nes path

        args -> error $ "args: " <> show args
  where
      path :: String -- default (for stack run)
      path = "data/dk.nes"
      --path = "data/nestest.nes"

speed :: String -> IO () -- test the speed of simulation (without gloss graphics)
speed path = SpeedTest.run path

nes :: String -> IO ()
nes path = Sim.Gloss.run path fs scale

fs :: Bool -- full-screen
fs = False

scale :: Int
scale = 2

dis :: String -> IO ()
dis path = do
    NesFile{prgs} <- loadNesFile path
    case prgs of
        [prg] ->
            disPRG 0xC000 prg
        [p1,p2] -> do
            disPRG 0x8000 p1
            disPRG 0xC000 p2
        _ ->
            error $ "dis, #prgs=" <> show (length prgs)

disPRG :: Addr -> PRG.ROM -> IO ()
disPRG addr prg = do
    let bytes = PRG.bytes prg
    let ops :: [Op] = decode bytes
    let bytes' = take (length bytes) $ reEncode ops -- in case 1 or 2 extra 0s
    when (bytes /= bytes') $ fail "re-assemble failed"
    mapM_ putStrLn $ displayOpLines addr ops

-- simple, non graphical entry point, used for nestest.nes regression test
emu :: String -> IO ()
emu path = do
    Sim.TraceCpu.printRun numSteps path
  where
    numSteps :: Int
    numSteps =
        case path of
            "data/nestest.nes" -> 5828 -- until reach unimplemented DCP
            _ -> 0
