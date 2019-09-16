
module Main (main) where

import Data.Word8 (Word8)
import Control.Monad (when)
import System.Environment (getArgs)
import qualified Data.ByteString as BS (ByteString,readFile,unpack)

import Six502.Values (Addr,addAddr,Byte(Byte))
import Six502.Operations (Op)
import Six502.Decode (decode,reEncode)
import Six502.Disassembler (displayOpLines)

import qualified Six502.Emu as Emu

main :: IO ()
main = do
    getArgs >>= \case
        ["--header"] -> header
        ["--dis"] -> dis
        ["--nestest"] -> emu
        args -> error $ "args: " <> show args

header :: IO () -- see header details
header = do
    bs <- loadFile "data/nestest.nes"
    putStrLn $ unwords $ map show (take headerSize bs)
    print ("length-file",length bs)
    print ("calc", show ((16 + 16*0x400 + 8*0x400) :: Int))
    print ("oneCodeChunk(16k)", show ((16*0x400)::Int))
    print ("sizeCode", sizeCode)

emu :: IO ()
emu = do
    bytesIncHeaderAndJunk <- loadFile "data/nestest.nes"
    let bytes = drop topSkip $ take sizeCode $ drop headerSize bytesIncHeaderAndJunk
    let limit = 5828 -- before reach unimplemented DCP
    let xs :: [Emu.State] = take limit $ Emu.run bytes
    mapM_ (putStrLn . Emu.showState) xs

dis :: IO ()
dis = do
    bytesIncHeaderAndJunk <- loadFile "data/nestest.nes"
    let bytes = drop topSkip $ take sizeCode $ drop headerSize bytesIncHeaderAndJunk
    let ops :: [Op] = decode bytes
    let bytes' = reEncode ops
    when (bytes /= bytes') $ fail "re-assemble failed"
    mapM_ putStrLn $ displayOpLines (startAddr `addAddr` topSkip) ops

startAddr :: Addr
startAddr = 0xC000

headerSize :: Int
headerSize = 16

sizeCode :: Int
sizeCode = 0x3B78

topSkip :: Int
topSkip = 0 --0x5F5

loadFile :: String -> IO [Byte]
loadFile path = do
    bs :: BS.ByteString <- BS.readFile path
    let ws :: [Word8] = BS.unpack bs
    return $ map Byte ws
