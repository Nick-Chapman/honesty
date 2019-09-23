
module PPU.Regs(
    Effect(..),
    Name(..),
    State, init, run,
    ) where

import Prelude hiding (init)
import Six502.Values
import qualified Ram2k

import Control.Monad (ap,liftM)

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

init :: State
init = State  -- what are correct init values?
    { control = 0x0
    , mask = 0x0
    , status = 0x80 -- lets set the vblank bit
    , ppu_addr_latch = Hi
    , ppu_addr_hi= 0x0
    , ppu_addr_lo = 0x0
    -- , ppu_data = 0x0
    }

data AddrLatch = Hi | Lo deriving (Show)

run :: State -> Effect a -> Ram2k.Effect (State, a) -- effect on VRAM
run state@State{control, mask, status
               ,ppu_addr_latch
               ,ppu_addr_hi, ppu_addr_lo
               } = \case
    Ret x -> return (state,x)
    Bind e f -> do
        (state',a) <- run state e
        run state' (f a)

    Read Control -> return (state,control)
    Read Mask -> return (state,mask)
    Read Status -> return (state,status)
    Read PPUADDR -> error "Read PPUADDR" -- return (state,ppu_addr)
    Read PPUDATA -> error "Read PPUDATA" -- return (state,ppu_data)
    Read OAMADDR -> error "Read OAMADDR"

    Write Control b -> return (state { control = b }, ())
    Write Mask b -> return (state { mask = b }, ())
    Write Status b -> return (state { status = b }, ())

    Write PPUADDR b -> do
        case ppu_addr_latch of
            Hi -> return (state { ppu_addr_hi = b, ppu_addr_latch = Lo }, ())
            Lo -> return (state { ppu_addr_lo = b, ppu_addr_latch = Hi }, ())

    Write PPUDATA b -> do
        -- return (state { ppu_data = b }, ())
        let a :: Addr = addrOfHiLo ppu_addr_hi ppu_addr_lo
        -- TODO: proper support for PPU MemMap & nametable mirroring
        Ram2k.Write (decode a) b
        return (bumpAddr state, ())

    Write OAMADDR _ -> do
        --error "Write OAMADDR"
        return (state, ()) -- TODO: dont ignore when handling sprites!

bumpAddr :: State -> State -- TODO: bump should be H/V depending on some status bit
bumpAddr s@State{ppu_addr_hi=hi, ppu_addr_lo=lo} = do
    let a = addrOfHiLo hi lo
    let (hi',lo') = addrToHiLo (a `addAddr` 1)
    s { ppu_addr_hi=hi', ppu_addr_lo=lo'}

decode :: Addr -> Int
decode a = if
    | a < 0x2000 -> error $ "Regs.decode, patten table: " <>  show a
    | a < 0x2800 ->  a `minusAddr` 0x2000
    | a < 0x3000 ->  a `minusAddr` 0x2800

    | a >= 0x3F00 && a < 0x3F1F ->
      error $ "Regs.decode, TODO: palette RAM index: " <> show a

    -- mirrors...
    | a < 0x3800 ->  a `minusAddr` 0x3000
    | a < 0x4000 ->  a `minusAddr` 0x3800

    | otherwise ->  error $ "Regs.decode, too high: " <> show a
