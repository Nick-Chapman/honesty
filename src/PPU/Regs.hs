
module PPU.Regs(
    Effect(..),
    Name(..),
    State, state0,
    setVBlank, isEnabledNMI,
    inter,
    ) where

import Control.Monad (ap,liftM)
import Data.Bits

import Six502.Values
import Six502.Cycles
import qualified Ram2k
import qualified CHR

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Name = Control | Mask | Status | PPUADDR | PPUDATA | OAMADDR-- | ...
    deriving Show

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Name -> Effect Byte
    Write :: Name -> Byte -> Effect ()

data State = State
    { control :: Byte
    , mask :: Byte
    , status :: Byte
    , ppu_addr_latch :: AddrLatch
    , ppu_addr_hi :: Byte
    , ppu_addr_lo :: Byte
    -- , ppu_data :: Byte
    } deriving Show

state0 :: State
state0 = State  -- what are correct init values?
    { control = 0x0
    , mask = 0x0
    , status = 0x80 -- lets set the vblank bit
    , ppu_addr_latch = Hi
    , ppu_addr_hi= 0x0
    , ppu_addr_lo = 0x0
    -- , ppu_data = 0x0
    }

setVBlank :: State -> Bool -> State
setVBlank state@State{control} bool =
    state { control = (if bool then setBit else clearBit) control 7 }

isEnabledNMI :: State -> Bool
isEnabledNMI State{control} = testBit control 7


data AddrLatch = Hi | Lo deriving (Show)

inter :: Cycles -> CHR.ROM -> State -> Effect a -> Ram2k.Effect (State, a) -- effect on VRAM
inter cc chr state@State{control, mask, status
               ,ppu_addr_latch
               ,ppu_addr_hi, ppu_addr_lo
               } = \case

    Ret x -> return (state,x)
    Bind e f -> do (state',a) <- inter cc chr state e; inter cc chr state' (f a)

    Read Control -> return (state,control)
    Read Mask -> return (state,mask)
    Read Status -> do
        let state' = state { ppu_addr_latch = Hi
                           , status = clearBit status 7 }
        return (state',status)

    Read PPUADDR -> error "Read PPUADDR"

    Read PPUDATA -> do
        let addr :: Addr = addrOfHiLo ppu_addr_hi ppu_addr_lo
        case decode cc addr of
            Rom x ->
                --error $ "TODO: Read PPUDADA, pattern table: " <>  show addr
                return (bumpAddr state, CHR.read chr x)
            Ram a -> do
                b <- Ram2k.Read a
                return (bumpAddr state, b)
            PaletteRam ->
                error "Read PPUDATA, PaletteRam"


    Read OAMADDR -> error "Read OAMADDR"

    Write Control b -> return (state { control = b }, ())
    Write Mask b -> return (state { mask = b }, ())
    Write Status b -> return (state { status = b }, ())

    Write PPUADDR b -> do
        case ppu_addr_latch of
            Hi -> return (state { ppu_addr_hi = b, ppu_addr_latch = Lo }, ())
            Lo -> return (state { ppu_addr_lo = b, ppu_addr_latch = Hi }, ())

    Write PPUDATA b -> do
        let addr :: Addr = addrOfHiLo ppu_addr_hi ppu_addr_lo
        case (decode cc addr) of
            Rom _ -> error $ "Write PPUDADA, cant write to pattern table: " <>  show addr
            Ram a -> Ram2k.Write a b
            PaletteRam -> return ()

        return (bumpAddr state, ())

    Write OAMADDR _ -> do
        --error "Write OAMADDR"
        return (state, ()) -- TODO: dont ignore when handling sprites!

bumpAddr :: State -> State -- TODO: bump should be H/V depending on some status bit
bumpAddr s@State{control,ppu_addr_hi=hi, ppu_addr_lo=lo} = do
    let bumpV = testBit control 2
    let bump = if bumpV then 32 else 1
    let a = addrOfHiLo hi lo
    let (hi',lo') = addrToHiLo (a `addAddr` bump)
    s { ppu_addr_hi=hi', ppu_addr_lo=lo'}

decode :: Cycles -> Addr -> Decode
decode cc a = if
    | a < 0x2000 -> Rom $ fromIntegral $ unAddr a
    | a < 0x2800 ->  Ram $ a `minusAddr` 0x2000
    | a < 0x3000 ->  Ram $ a `minusAddr` 0x2800

    | a >= 0x3F00 && a <= 0x3F1F -> PaletteRam

    -- mirrors... wait and see if they are used
--    | a < 0x3800 ->  a `minusAddr` 0x3000
--    | a < 0x4000 ->  a `minusAddr` 0x3800

    | otherwise ->  error $ show cc <> " - Regs.decode, too high: " <> show a

data Decode
    = Ram Int
    | Rom Int
    | PaletteRam
