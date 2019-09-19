
module Main (main) where

import Control.Monad (when)
import Data.Tuple.Extra( (***) )
import Data.Word8 (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString as BS (ByteString,readFile,unpack)

import Six502.Decode (decode,reEncode)
import Six502.Disassembler (displayOpLines)
import Six502.Operations (Op)
import Six502.Values (Byte(Byte),bytesToString,byteToUnsigned)

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
        ["--header"] -> header path
        ["--header",path] -> header path
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

header :: String -> IO () -- explore header
header path = do
    nf <- loadNesFile path
    print nf

emu :: String -> IO ()
emu path = do
    bytesIncHeaderAndJunk <- loadFile path
    let bytes = drop headerSize bytesIncHeaderAndJunk
    let xs :: [Emu.State] = limitEmuSteps $ Emu.run codeLoadAddr codeStartAddr bytes
    mapM_ (putStrLn . Emu.showState) xs
    where
      codeLoadAddr = 0xC000
      codeStartAddr =
          case path of
              "data/dk.nes" -> 0xC79E
              _ -> 0xC000
      limitEmuSteps =
          case path of
              "data/nestest.nes" -> take 5828 -- until reach unimplemented DCP
              _ -> id

dis :: String -> IO ()
dis path = do
    bytesIncHeaderAndJunk <- loadFile path
    let bytes = takeCode $ drop headerSize bytesIncHeaderAndJunk
    let ops :: [Op] = decode bytes
    let bytes' = reEncode ops
    when (bytes /= bytes') $ fail "re-assemble failed"
    mapM_ putStrLn $ displayOpLines startAddr ops
    where
        startAddr = 0xC000
        takeCode = case path of
            "data/nestest.nes" -> take 0x3B78
            _ -> id

headerSize :: Int
headerSize = 16

type PRG = [Byte]

prgSize :: Int
prgSize = 0x4000 --16k

chrSize :: Int
chrSize = 0x1000 --2k (One CHR of 256 tiles)

data NesFile = NesFile { prgs :: [PRG], chrs :: [(CHR,CHR)] }

instance Show NesFile where
    show NesFile{prgs,chrs} =
        unwords ["NesFile:"
                ,"#prgs=" <> show (length prgs)
                ,"#chrs=" <> show (length chrs)]

loadNesFile :: String -> IO NesFile
loadNesFile path = do
    bs <- loadFile path
    putStrLn $ unwords $ map show (take headerSize bs)
    when (length bs < headerSize) $ error "header failure, too short"
    when (bytesToString (take 3 bs) /= "NES") $ error "header failure, missing NES tag"
    let x = byteToUnsigned (bs !! 4)
    let y = byteToUnsigned (bs !! 5)
    when (length bs /= headerSize + (x * prgSize) + (y * 2 * chrSize)) $ error "bad file size"
    let prgs = map (\i -> take prgSize $ drop (headerSize + i * prgSize) bs) [0..x-1]
    let chrs = map (\i -> chrPairFromBS $ take (2*chrSize) $ drop (headerSize + x * prgSize + i * 2 * chrSize) bs) [0..y-1]
    return $ NesFile prgs chrs

chrPairFromBS :: [Byte] -> (CHR,CHR)
chrPairFromBS = (Nes.chrFromBS *** Nes.chrFromBS) . splitAt chrSize

loadFile :: String -> IO [Byte]
loadFile path = do
    bs :: BS.ByteString <- BS.readFile path
    let ws :: [Word8] = BS.unpack bs
    return $ map Byte ws
