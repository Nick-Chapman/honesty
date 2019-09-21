
module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs)

import System(State,state0,stepCPU)
import Six502.Values
import Six502.Decode (decode,reEncode)
import Six502.Disassembler (displayOpLines)
import Six502.Operations (Op)
import NesFile
import Sim(gloss)
import qualified PRG

main :: IO ()
main = do
    getArgs >>= \case
        ["--dis"] -> dis path
        ["--dis",path] -> dis path

        -- Just see CPU states
        ["--emu"] -> emu path
        ["--emu",path] -> emu path

        -- WIP, fuller NES emulation (but really only 6502 still)
        [] -> nes path
        [path] -> nes path

        args -> error $ "args: " <> show args
  where
      path :: String -- default (for stack run)
      path = "data/dk.nes"
      --path = "data/nestest.nes"

nes :: String -> IO ()
nes path = Sim.gloss path fg scale

fg :: Bool
fg = False

scale :: Int
scale = 1

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
    let bytes = PRG.unROM prg
    let ops :: [Op] = decode bytes
    let bytes' = take (length bytes) $ reEncode ops -- in case 1 or 2 extra 0s
    when (bytes /= bytes') $ fail "re-assemble failed"
    mapM_ putStrLn $ displayOpLines addr ops


-- simple, non graphical entry point, used for nestest.nes regression test
emu :: String -> IO ()
emu path = do
    state <- state0 path
    let states :: [State] = limitEmuSteps path $ run state
    mapM_ print states
  where
    run :: State -> [State]
    run state = state : run (stepCPU state)

    limitEmuSteps :: String -> [a] -> [a]
    limitEmuSteps path =
        case path of
            "data/nestest.nes" -> take 5828 -- until reach unimplemented DCP
            _ -> id
