
module Main (main) where

import Control.Monad (when)
import Data.Tuple.Extra( (***) )
import System.Environment (getArgs)
import qualified Data.ByteString as BS (readFile,unpack)

import Six502.Decode (decode,reEncode)
import Six502.Disassembler (displayOpLines)
import Six502.Operations (Op)
import Six502.Values (Addr,Byte(Byte),bytesToString,byteToUnsigned,addrOfHiLo)

import qualified Six502.Emu as Emu
import qualified Nes
import Nes(CHR)

main :: IO ()
main = do
    getArgs >>= \case
        ["--dis"] -> dis path
        ["--dis",path] -> dis path
        ["--emu"] -> emu path
        ["--emu",path] -> emu path
        ["--header"] -> xheader path
        ["--header",path] -> xheader path
        ["--tiles"] -> tiles path
        ["--tiles",path] -> tiles path
        args -> error $ "args: " <> show args
  where
      path :: String
      path = "data/dk.nes"

fg :: Bool
fg = False

scale :: Int
scale = 4

tiles :: String -> IO () -- see CHR data graphically
tiles path = do
    NesFile{chrs=[(chr1,chr2)]} <- loadNesFile path
    Nes.glossMainShowChr fg scale (chr1,chr2)

xheader :: String -> IO () -- explore header
xheader path = do
    nf <- loadNesFile path
    print nf

emu :: String -> IO ()
emu path = do
    NesFile{prgs} <- loadNesFile path
    f prgs
    where
        f = \case
            [prg@(PRG bytes)] -> do
                let codeLoadAddr = 0xC000
                let codeStartAddr = resetAddr prg
                let xs :: [Emu.State] = limitEmuSteps $ Emu.run codeLoadAddr codeStartAddr bytes
                mapM_ (putStrLn . Emu.showState) xs

            [PRG bytes1,prg@(PRG bytes2)] -> do
                let codeLoadAddr = 0x8000
                let codeStartAddr = resetAddr prg
                let bytes = bytes1 ++ bytes2
                let xs :: [Emu.State] = limitEmuSteps $ Emu.run codeLoadAddr codeStartAddr bytes
                mapM_ (putStrLn . Emu.showState) xs

            _ -> error "emu"

        resetAddr (PRG bytes) =
            case path of
                "data/nestest.nes" -> 0xC000
                _ -> addrOfHiLo hi lo
                    where
                        lo = bytes !! 0x3ffc
                        hi = bytes !! 0x3ffd

        limitEmuSteps =
            case path of
                "data/nestest.nes" -> take 5828 -- until reach unimplemented DCP
                _ -> id

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

disPRG :: Addr -> PRG -> IO ()
disPRG addr (PRG bytes) = do
    let ops :: [Op] = decode bytes
    let bytes' = take (length bytes) $ reEncode ops -- in case 1 or 2 extra 0s
    when (bytes /= bytes') $ fail "re-assemble failed"
    mapM_ putStrLn $ displayOpLines addr ops

headerSize :: Int
headerSize = 16

newtype PRG = PRG [Byte]

prgSize :: Int
prgSize = 0x4000 --16k

chrSize :: Int
chrSize = 0x1000 --2k (One CHR of 256 tiles)

data NesFile = NesFile { header :: [Byte], prgs :: [PRG], chrs :: [(CHR,CHR)] }

instance Show NesFile where
    show NesFile{header} = "NesFile: " <> (unwords $ map show header)

loadNesFile :: String -> IO NesFile
loadNesFile path = do
    byteString <- BS.readFile path
    let bs = map Byte $ BS.unpack byteString
    when (length bs < headerSize) $ error "header failure, too short"
    when (bytesToString (take 3 bs) /= "NES") $ error "header failure, missing NES tag"
    let header = take headerSize bs
    let x = byteToUnsigned (bs !! 4)
    let y = byteToUnsigned (bs !! 5)
    when (length bs /= headerSize + (x * prgSize) + (y * 2 * chrSize)) $ error "bad file size"
    let prgs = map (\i -> PRG $ take prgSize $ drop (headerSize + i * prgSize) bs) [0..x-1]
    let chrs = map (\i -> chrPairFromBS $ take (2*chrSize) $ drop (headerSize + x * prgSize + i * 2 * chrSize) bs) [0..y-1]
    return $ NesFile header prgs chrs

chrPairFromBS :: [Byte] -> (CHR,CHR)
chrPairFromBS = (Nes.chrFromBS *** Nes.chrFromBS) . splitAt chrSize
