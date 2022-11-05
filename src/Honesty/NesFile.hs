
module Honesty.NesFile(
    loadNesFile, NesFile(..),
    ) where

import Control.Monad (when)
import Data.Bits (testBit)
import Honesty.Byte (Byte(..),bytesToString,byteToUnsigned)
import Honesty.PPU.PMem(NametableMirroring(..))
import qualified Data.ByteString as BS (readFile,unpack)
import qualified Honesty.CHR as CHR (ROM,init)
import qualified Honesty.PRG as PRG (ROM,init)

headerSize :: Int
headerSize = 16

prgSize :: Int
prgSize = 0x4000 --16k

patSize :: Int
patSize = 0x1000 --2k (One PAT of 256 tiles)

chrSize :: Int
chrSize = 2 * patSize --4k

data NesFile = NesFile
    { header :: [Byte]
    , prgs :: [PRG.ROM]
    , chrs :: [CHR.ROM]
    , ntm :: NametableMirroring
    }

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
    let ntm = if byteToUnsigned (bs !! 6) `testBit` 0 then NTM_Horizontal else NTM_Vertical
    when (length bs /= headerSize + (x * prgSize) + (y * chrSize)) $ error "bad file size"
    let prgs = map (\i -> PRG.init $ take prgSize $ drop (headerSize + i * prgSize) bs) [0..x-1]
    let chrs = map (\i -> CHR.init $ take chrSize $ drop (headerSize + x * prgSize + i * 2 * patSize) bs) [0..y-1]
    return $ NesFile { header,  prgs,  chrs, ntm }
