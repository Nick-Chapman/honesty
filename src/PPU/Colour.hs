
module PPU.Colour(
    Colour,
    ofByte,
    toRGB,
    ) where

import Data.Array(Array,(!),listArray)
import Data.Word

import Byte

newtype Colour = Colour Word32

toRGB :: Colour -> Word32
toRGB (Colour w) = w

ofByte :: Byte -> Colour
ofByte byte = do
    let i = fromIntegral $ unByte byte
    if i>=64 then error $ "Colour.toGloss(>=64)," <> show i else do
    Colour (wordMapping ! i)

wordMapping :: Array Int Word32
wordMapping = listArray (0,63)
    [0x7C7C7Cff
    ,0x0000FCff
    ,0x0000BCff
    ,0x4428BCff
    ,0x940084ff
    ,0xA80020ff
    ,0xA81000ff
    ,0x881400ff
    ,0x503000ff
    ,0x007800ff
    ,0x006800ff
    ,0x005800ff
    ,0x004058ff
    ,0x000000ff
    ,0x000000ff
    ,0x000000ff
    ,0xBCBCBCff
    ,0x0078F8ff
    ,0x0058F8ff
    ,0x6844FCff
    ,0xD800CCff
    ,0xE40058ff
    ,0xF83800ff
    ,0xE45C10ff
    ,0xAC7C00ff
    ,0x00B800ff
    ,0x00A800ff
    ,0x00A844ff
    ,0x008888ff
    ,0x000000ff
    ,0x000000ff
    ,0x000000ff
    ,0xF8F8F8ff
    ,0x3CBCFCff
    ,0x6888FCff
    ,0x9878F8ff
    ,0xF878F8ff
    ,0xF85898ff
    ,0xF87858ff
    ,0xFCA044ff
    ,0xF8B800ff
    ,0xB8F818ff
    ,0x58D854ff
    ,0x58F898ff
    ,0x00E8D8ff
    ,0x787878ff
    ,0x000000ff
    ,0x000000ff
    ,0xFCFCFCff
    ,0xA4E4FCff
    ,0xB8B8F8ff
    ,0xD8B8F8ff
    ,0xF8B8F8ff
    ,0xF8A4C0ff
    ,0xF0D0B0ff
    ,0xFCE0A8ff
    ,0xF8D878ff
    ,0xD8F878ff
    ,0xB8F8B8ff
    ,0xB8F8D8ff
    ,0x00FCFCff
    ,0xF8D8F8ff
    ,0x000000ff
    ,0x000000ff
    ]
