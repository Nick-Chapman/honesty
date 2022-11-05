
module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs)

import Honesty.Addr (Addr)
import Honesty.NesFile (NesFile(NesFile,prgs),loadNesFile)
import Honesty.Six502.Decode (decode,reEncode)
import Honesty.Six502.Disassembler (displayOpLines)
import Honesty.Six502.Operations (Op)
import qualified Honesty.PRG as PRG (ROM,bytes)
import qualified Honesty.Gloss as Gloss(Size(..),run)
import qualified Honesty.TraceCpu as TraceCpu(printRun,printRunBLA)
import qualified Honesty.SpeedTest as SpeedTest(run)

main :: IO ()
main = do
    args <- getArgs
    let conf = parseArgs args
    runConf conf

data Mode
    = Disassemble6502
    | Emulate6502
    | Emulate6502_Blargg
    | SpeedTestNes
    | GlossNes

data Conf = Conf
    { mode :: Mode
    , size :: Gloss.Size        -- only for Gloss
    , fps :: Int                -- only for Gloss
    , optMaxFrames :: Maybe Int -- only for SpeedTest
    , debug :: Bool
    , path :: String
    }

defaultConf :: Conf
defaultConf = Conf
    { mode = GlossNes
    , size = Gloss.Normal
    , fps = 30                  -- try 45 on a faster machine
    , optMaxFrames = Nothing
    , debug = False
    , path = "data/dk.nes"
    }

parseArgs :: [String] -> Conf
parseArgs args = loop args defaultConf
    where
        loop args conf = case args of
            [] -> conf
            "--dis":rest -> loop rest $ conf { mode = Disassemble6502 }
            "--emu":rest -> loop rest $ conf { mode = Emulate6502 }
            "--bla":rest -> loop rest $ conf { mode = Emulate6502_Blargg }
            "--speed":rest -> loop rest $ conf { mode = SpeedTestNes }
            "--tiny":rest -> loop rest $ conf { size = Gloss.Tiny }
            "--full":rest -> loop rest $ conf { size = Gloss.Full }
            "--fps":n:rest -> loop rest $ conf { fps = read n }
            "--max-frames":n:rest -> loop rest $ conf { optMaxFrames = Just (read n) }
            "--debug":rest -> loop rest $ conf { debug = True }
            path:rest -> loop rest $ conf { path }

runConf :: Conf -> IO ()
runConf = \case
    Conf{mode=Disassemble6502,path} -> dis path
    Conf{mode=Emulate6502,path} -> emu path
    Conf{mode=Emulate6502_Blargg,path} -> blaarg path
    Conf{mode=SpeedTestNes,path,optMaxFrames} -> SpeedTest.run path optMaxFrames
    Conf{mode=GlossNes,size,path,fps,debug} -> Gloss.run path size fps debug

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
    TraceCpu.printRun numSteps path
  where
    numSteps :: Int
    numSteps =
        -- TODO: avoid this hack by passing as parameter from ./test.sh
        case path of
            "data/nestest.nes" -> 5828 -- until reach unimplemented DCP
            _ -> 0

blaarg :: String -> IO ()
blaarg path = do
    TraceCpu.printRunBLA 0 path
