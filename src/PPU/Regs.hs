
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

data Name = Control | Mask | Status -- | ...
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
    } deriving Show

init :: State
init = State { control = 0x0, mask = 0x0, status = 0x0  } -- what are correct init values?

run :: State -> Effect a -> (State, a)
run state@State{control,mask,status} = \case
    Ret x -> (state,x)
    Bind e f -> let (state',a) = run state e in run state' (f a)

    Read Control -> (state,control)
    Read Mask -> (state,mask)
    Read Status -> (state,status)

    Write Control b -> (state { control = b }, ())
    Write Mask b -> (state { mask = b }, ())
    Write Status b -> (state { status = b }, ())
