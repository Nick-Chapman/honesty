{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Six502.Decode (
    Op(..),
    Rand(..),
    Addr(..),
    decode1,
    decodeOps,
    reEncodeOps,
    opBytes,
    sizeMode,
    byteToSigned,
    addAddr,
    minusAddr,
    zeroPageAddr,
    page1Addr,
    addrToHiLo,
    addrOfHiLo,
    addByte,
    ) where

import Data.Word (Word16)
import Control.Monad (join)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

import Six502.Types (Byte(Byte,unByte),Instruction(..),Mode(..))
import qualified Six502.OpCode as OpCode(table)

data Op
    = Unknown [Byte]
    | Op Instruction Mode Rand
    deriving (Show)

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

data Rand = RandByte Byte | RandAddr Addr | RandNull deriving (Show)

randBytes :: Rand -> [Byte]
randBytes = \case
    RandByte b -> [b]
    RandAddr a -> [lo,hi] where (hi,lo) = addrToHiLo a
    RandNull -> []

takeMode :: Mode -> [Byte] -> (Rand,[Byte])
takeMode = snd . specMode

sizeMode :: Mode -> Int
sizeMode = fst . specMode

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

addrOfHiLo :: Byte -> Byte -> Addr
addrOfHiLo hi lo =
    Addr (256 * fromIntegral (unByte hi) + fromIntegral (unByte lo))

addrToHiLo :: Addr -> (Byte,Byte)
addrToHiLo a = (hi,lo) where -- do it with Bits instead?
    lo = byteOfInt $ n `mod` 256
    hi = byteOfInt $ n `div` 256
    n = fromIntegral $ unAddr a

byteOfInt :: Int -> Byte
byteOfInt = Byte . fromIntegral

byteToUnsigned :: Byte -> Int
byteToUnsigned = fromIntegral . unByte

byteToSigned :: Byte -> Int
byteToSigned = sign . fromIntegral . unByte
    where sign n = if n>=128 then n-256 else n

addByte :: Byte -> Int -> Byte
addByte a n = Byte (unByte a + fromIntegral n)


newtype Addr = Addr { unAddr :: Word16 } deriving (Eq,Ord,Num)

instance Show Addr where show = printf "%04X" . unAddr

addAddr :: Addr -> Int -> Addr
addAddr a n = Addr (unAddr a + fromIntegral n)

minusAddr :: Addr -> Addr -> Int
minusAddr later earlier = fromIntegral (unAddr later - unAddr earlier)

zeroPageAddr :: Byte -> Addr
zeroPageAddr b = Addr $ fromIntegral $ byteToUnsigned b

page1Addr :: Byte -> Addr
page1Addr b = Addr $ 256 + (fromIntegral $ byteToUnsigned b)
