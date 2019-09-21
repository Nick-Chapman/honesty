
module NesFile(
    loadNesFile, NesFile(..),
    ) where

import Control.Monad (when)
import Data.Tuple.Extra( (***) )
import qualified Data.ByteString as BS (readFile,unpack)

import Six502.Values

import Graphics(CHR,chrFromBS)

import qualified PRG

headerSize :: Int
headerSize = 16

prgSize :: Int
prgSize = 0x4000 --16k

chrSize :: Int
chrSize = 0x1000 --2k (One CHR of 256 tiles)

data NesFile = NesFile { header :: [Byte], prgs :: [PRG.ROM], chrs :: [(CHR,CHR)] }

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
    let prgs = map (\i -> PRG.init $ take prgSize $ drop (headerSize + i * prgSize) bs) [0..x-1]
    let chrs = map (\i -> chrPairFromBS $ take (2*chrSize) $ drop (headerSize + x * prgSize + i * 2 * chrSize) bs) [0..y-1]
    return $ NesFile header prgs chrs

chrPairFromBS :: [Byte] -> (CHR,CHR)
chrPairFromBS = (chrFromBS *** chrFromBS) . splitAt chrSize
