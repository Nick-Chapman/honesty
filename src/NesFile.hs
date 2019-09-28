
module NesFile(
    loadNesFile, NesFile(..),
    ) where

import Control.Monad (when)
import Data.Tuple.Extra( (***) )
import qualified Data.ByteString as BS (readFile,unpack)

import Six502.Values

import Graphics(PAT,patFromBS)

import qualified PRG

headerSize :: Int
headerSize = 16

prgSize :: Int
prgSize = 0x4000 --16k

patSize :: Int
patSize = 0x1000 --2k (One PAT of 256 tiles)

data NesFile = NesFile { header :: [Byte], prgs :: [PRG.ROM], pats :: [(PAT,PAT)] }

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
    when (length bs /= headerSize + (x * prgSize) + (y * 2 * patSize)) $ error "bad file size"
    let prgs = map (\i -> PRG.init $ take prgSize $ drop (headerSize + i * prgSize) bs) [0..x-1]
    let pats = map (\i -> patPairFromBS $ take (2*patSize) $ drop (headerSize + x * prgSize + i * 2 * patSize) bs) [0..y-1]
    return $ NesFile header prgs pats

patPairFromBS :: [Byte] -> (PAT,PAT)
patPairFromBS = (patFromBS *** patFromBS) . splitAt patSize
