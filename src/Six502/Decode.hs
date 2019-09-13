
module Six502.Decode (
    decode1,
    decodeOps,
    reEncodeOps,
    opBytes,
    opSize,
    ) where

import Control.Monad (join)
import Data.Maybe (mapMaybe)

import Six502.Values
import Six502.Operations
import qualified Six502.OpCode as OpCode(table)

reEncodeOps :: [Op] -> [Byte]
reEncodeOps = join . map opBytes

decodeViaTable :: Byte -> Maybe (Instruction,Mode)
decodeViaTable byte = -- TODO: be more efficient!
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
    Unknown bs -> bs
    Op instruction mode rand -> encodeViaTable (instruction,mode) : randBytes rand

decode1 :: [Byte] -> Op
decode1 = head . decodeOps

decodeOps :: [Byte] -> [Op]
decodeOps = dis []

dis :: [Byte] -> [Byte] -> [Op]
dis junk = \case
    [] -> flush []
    b:bs ->
        case decodeViaTable b of
            Nothing -> dis (b:junk) bs
            Just (instruction,mode) ->
                flush $ Op instruction mode rand : dis [] bs' where (rand,bs') = takeMode mode bs
    where
        flush :: [Op] -> [Op]
        flush ops = case junk of
            [] -> ops
            _ -> Unknown (reverse junk) : ops

randBytes :: Rand -> [Byte]
randBytes = \case
    RandByte b -> [b]
    RandAddr a -> [lo,hi] where (hi,lo) = addrToHiLo a
    RandNull -> []

takeMode :: Mode -> [Byte] -> (Rand,[Byte])
takeMode = snd . specMode

opSize :: Op -> Int
opSize = \case
    Unknown xs -> length xs
    Op _ mode _ -> 1 + modeSize mode

modeSize :: Mode -> Int
modeSize = fst . specMode

type ModeSpec = (Int, [Byte] -> (Rand,[Byte]))

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
spec0 = (0, \bs -> (RandNull, bs))

spec1 :: ModeSpec
spec1 = (1, \case b:rest -> (RandByte b, rest); _ -> error "spec1")

spec2 :: ModeSpec
spec2 = (2, \case lo:hi:rest -> (RandAddr $ addrOfHiLo hi lo, rest); _ -> error "take2")
