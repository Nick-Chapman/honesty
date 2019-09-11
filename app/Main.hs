{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import qualified Data.ByteString as BS
import Data.Word8
import Control.Monad

import Six502.Types (Byte(..))
import Six502.Decode (Op,Addr,decodeOps,reEncodeOps,addAddr)
import Six502.Disassembler (displayOpLines)

main :: IO ()
main = do
    bytesIncHeaderAndJunk <- loadFile "data/nestest.nes"
    let bytes = drop topSkip $ take sizeCode $ drop headerSize bytesIncHeaderAndJunk
    let ops :: [Op] = decodeOps bytes
    let bytes' = reEncodeOps ops
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
