
module PPU.Regs(
    Effect(..),
    Name(..),
    State, init, run,
    ) where

import Prelude hiding (init)
import Six502.Values

import Control.Monad (ap,liftM)

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Name = Control | Mask | Status | PPUADDR | PPUDATA -- | ...
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
    , ppu_addr :: Byte
    , ppu_data :: Byte
    } deriving Show

init :: State
init = State  -- what are correct init values?
    { control = 0x0
    , mask = 0x0
    , status = 0x80 -- lets set the vblank bit
    , ppu_addr = 0x0
    , ppu_data = 0x0
    }


-- TODO: This should have an effect in the PPU MemMap
run :: State -> Effect a -> (State, a)
run state@State{control,mask,status,ppu_addr,ppu_data} = \case
    Ret x -> (state,x)
    Bind e f -> let (state',a) = run state e in run state' (f a)

    Read Control -> (state,control)
    Read Mask -> (state,mask)
    Read Status -> (state,status)
    Read PPUADDR -> (state,ppu_addr)
    Read PPUDATA -> (state,ppu_data)

    Write Control b -> (state { control = b }, ())
    Write Mask b -> (state { mask = b }, ())
    Write Status b -> (state { status = b }, ())
    Write PPUADDR b -> (state { ppu_addr = b }, ())
    Write PPUDATA b -> (state { ppu_data = b }, ())
