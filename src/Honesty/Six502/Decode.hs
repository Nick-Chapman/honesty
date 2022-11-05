
module Honesty.Six502.Decode (
    decode1,
    decode,
    reEncode,
    opBytes,
    opSize,
    ) where

import Control.Monad (join)
import Data.Array ((!),listArray)
import Data.Maybe (mapMaybe)
import Honesty.Addr (addrToHiLo,addrOfHiLo)
import Honesty.Byte (Byte)
import Honesty.Six502.Operations (Op(..),Instruction,Mode(..),Arg(..))
import qualified Honesty.Six502.OpCode as OpCode(table)

reEncode :: [Op] -> [Byte]
reEncode = join . map opBytes

decodeViaTable :: Byte -> Maybe (Instruction,Mode)
decodeViaTable = if
    | quick -> (arr !)
    | otherwise -> raw_decodeViaTable
    where
        quick = True -- fps: 15 --> 21
        arr = listArray (0,255) $ map raw_decodeViaTable [0..255]
        raw_decodeViaTable byte =
            case mapMaybe (\(i,m,b) -> if byte==b then Just (i,m) else Nothing) OpCode.table of
                [] -> Nothing
                [im] -> Just im
                ims -> error $ "decodeViaTable:" <> show byte <> " -> " <> show ims

encodeViaTable :: (Instruction,Mode) -> Byte
encodeViaTable (instruction,mode) = -- TODO: be more efficient!
    case mapMaybe (\(i,m,b) -> if instruction==i && mode==m then Just b else Nothing) OpCode.table of
        [] -> error $ "encodeViaTable" <> show (instruction,mode)
        [b] -> b
        bs -> error $ "encodeViaTable:" <> show (instruction,mode) <> " -> " <> show bs

opBytes :: Op -> [Byte]
opBytes = \case
    Unknown b -> [b]
    Op instruction mode rand -> encodeViaTable (instruction,mode) : randBytes rand

decode1 :: [Byte] -> Op
decode1 = \case
    [] -> error "decode1,[]"
    b:bs ->
        case decodeViaTable b of
            Nothing -> Unknown b
            Just (instruction,mode) ->
                Op instruction mode rand where (rand,_) = takeMode mode bs

decode :: [Byte] -> [Op]
decode = \case
    [] -> []
    bs -> do
        let op = decode1 bs
        op : decode (drop (opSize op) bs)

randBytes :: Arg -> [Byte]
randBytes = \case
    ArgByte b -> [b]
    ArgAddr a -> [lo,hi] where (hi,lo) = addrToHiLo a
    ArgNull -> []

takeMode :: Mode -> [Byte] -> (Arg,[Byte])
takeMode = snd . specMode

opSize :: Op -> Int
opSize = \case
    Unknown _ -> 1
    Op _ mode _ -> 1 + modeSize mode

modeSize :: Mode -> Int
modeSize = fst . specMode

type ModeSpec = (Int, [Byte] -> (Arg,[Byte]))

specMode :: Mode -> ModeSpec
specMode = \case
    Immediate -> spec1
    ZeroPage -> spec1
    Relative -> spec1
    Absolute -> spec2
    Implied -> spec0
    AbsoluteX -> spec2
    AbsoluteY -> spec2
    ZeroPageX -> spec1
    ZeroPageY -> spec1
    IndexedIndirect -> spec1
    IndirectIndexed -> spec1
    Accumulator -> spec0
    Indirect -> spec2

spec0 :: ModeSpec
spec0 = (0, \bs -> (ArgNull, bs))

spec1 :: ModeSpec
spec1 = (1, \case b:rest -> (ArgByte b, rest); _ -> (ArgByte 0,[]))

spec2 :: ModeSpec
spec2 = (2, \case lo:hi:rest -> (ArgAddr $ addrOfHiLo hi lo, rest); _ -> (ArgAddr 0,[]))
