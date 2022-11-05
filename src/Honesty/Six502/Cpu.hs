
module Honesty.Six502.Cpu(
    State(..), state0,
    ) where

import Honesty.Addr (Addr)
import Honesty.Byte (Byte)

data State = State
    { pc :: !Addr
    , accumulator :: !Byte
    , xreg :: !Byte
    , yreg :: !Byte
    , status :: Byte
    , sp :: !Byte
    }

state0 :: Addr -> State
state0 pc = State
    { pc
    , accumulator = 0
    , xreg = 0
    , yreg = 0
    , sp = 0xFD
    , status = 0x24
    }

instance Show State where
    show = \State{accumulator,xreg,yreg,status,sp} -> unwords
        [ "A:" <> show accumulator
        , "X:" <> show xreg
        , "Y:" <> show yreg
        , "P:" <> show status
        , "SP:" <> show sp
        ]
