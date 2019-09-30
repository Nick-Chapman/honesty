
module PPU.Regs(
    Effect(..),
    Name(..),
    State, state0,
    setVBlank, isEnabledNMI,
    inter,
    ) where

import Data.Bits

import Byte
import Addr
import Six502.Cycles
import qualified CHR
import qualified PPU.PMem as PMem

import Control.Monad (ap,liftM)
instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Name
    = PPUCTRL
    | PPUMASK
    | PPUSTATUS
    | OAMADDR
    | OAMDATA
    | PPUSCROLL
    | PPUADDR
    | PPUDATA
    | OAMDMA

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Read :: Name -> Effect Byte
    Write :: Name -> Byte -> Effect ()

data State = State
    { control :: Byte
    , mask :: Byte
    , status :: Byte
    , addr_latch :: AddrLatch
    , addr_hi :: Byte
    , addr_lo :: Byte
    } deriving Show

state0 :: State
state0 = State
    { control = 0x0
    , mask = 0x0
    , status = 0x80 -- why must this be set?
    , addr_latch = Hi
    , addr_hi= 0x0
    , addr_lo = 0x0
    }

setVBlank :: State -> Bool -> State
setVBlank state@State{control} bool =
    state { control = (if bool then setBit else clearBit) control 7 }

isEnabledNMI :: State -> Bool
isEnabledNMI State{control} = testBit control 7

data AddrLatch = Hi | Lo deriving (Show)

inter :: Cycles -> CHR.ROM -> State -> Effect a -> PMem.Effect (State, a)
inter cc chr state@State{control, mask, status ,addr_latch ,addr_hi, addr_lo} = \case

    Ret x -> return (state,x)
    Bind e f -> do (state',a) <- inter cc chr state e; inter cc chr state' (f a)

    Read PPUCTRL -> return (state,control)
    Read PPUMASK -> return (state,mask)
    Read PPUSTATUS -> do
        let state' = state { addr_latch = Hi , status = clearBit status 7 }
        return (state',status)
    Read OAMADDR -> error "Read OAMADDR"
    Read OAMDATA -> error "Read OAMDATA"
    Read PPUSCROLL -> error "Read PPUSCROLL"
    Read PPUADDR -> error "Read PPUADDR"
    Read PPUDATA -> do
        let addr = addrOfHiLo addr_hi addr_lo
        b <- PMem.Read addr
        return (bumpAddr state, b)
    Read OAMDMA -> error "Read OAMDMA"

    Write PPUCTRL b -> return (state { control = b }, ())
    Write PPUMASK b -> return (state { mask = b }, ())
    Write PPUSTATUS b -> return (state { status = b }, ())
    Write OAMADDR _ -> do return (state, ()) -- TODO: dont ignore when handling sprites!
    Write OAMDATA _ -> error "Write OAMDATA"
    Write PPUSCROLL _ -> do return (state, ()) -- TODO: dont ignore for scrolling
    Write PPUADDR b -> do
        case addr_latch of
            Hi -> return (state { addr_hi = b, addr_latch = Lo }, ())
            Lo -> return (state { addr_lo = b, addr_latch = Hi }, ())
    Write PPUDATA b -> do
        let addr = addrOfHiLo addr_hi addr_lo
        PMem.Write addr b
        return (bumpAddr state, ())
    Write OAMDMA _ -> do return (state, ()) -- error "Write OAMDMA" - TODO NEXT

bumpAddr :: State -> State
bumpAddr s@State{control,addr_hi=hi, addr_lo=lo} = do
    let bumpV = testBit control 2
    let bump = if bumpV then 32 else 1
    let a = addrOfHiLo hi lo
    let (hi',lo') = addrToHiLo (a `addAddr` bump)
    s { addr_hi=hi', addr_lo=lo'}
