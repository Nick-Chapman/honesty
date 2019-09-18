
module Main (main) where

import Control.Monad (when)
import Data.Word8 (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString as BS (ByteString,readFile,unpack)

import Six502.Decode (decode,reEncode)
import Six502.Disassembler (displayOpLines)
import Six502.Operations (Op)
import Six502.Values (Byte(Byte))

import qualified Six502.Emu as Emu
import qualified Nes

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

tiles :: String -> IO () -- see CHR data graphically
tiles path = do
    bs <- loadFile path
    let chr = Nes.chrFromBS $ take 0x1000 $ drop 0x4000 $ drop headerSize bs
    Nes.glossMainShowChr chr

header :: String -> IO () -- explore header
header path = do
    bs <- loadFile path
    putStrLn $ unwords $ map show (take headerSize bs)
    print ("length-file",length bs)
    print ("calc", show ((16 + 16*0x400 + 8*0x400) :: Int))

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

loadFile :: String -> IO [Byte]
loadFile path = do
    bs :: BS.ByteString <- BS.readFile path
    let ws :: [Word8] = BS.unpack bs
    return $ map Byte ws
