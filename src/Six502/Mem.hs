
module Six502.Mem( -- designed for qualified import
    State,
    initializeWithCode,
    Effect(..),
    run,
    ) where

import Control.Monad (ap,liftM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Six502.Values

data State = State
    { codeLoadAddr :: Addr
    , code :: [Byte]
    , ram :: Map Addr Byte
    }

initializeWithCode :: Addr -> [Byte] -> State
initializeWithCode codeLoadAddr code = State {codeLoadAddr, code, ram = Map.empty}

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Reads :: Addr -> Effect [Byte]
    Store :: Addr -> Byte -> Effect ()

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

run :: Effect a -> State -> (State,a)
run eff mem =
    case eff of
        Ret a -> (mem,a)
        Bind m f -> do
            let (mem',a) = run m mem
            run (f a) mem'
        Reads addr -> (mem, memReads mem addr)
        Store addr byte -> (memStore mem addr byte,())

memReads :: State -> Addr -> [Byte]
memReads State{codeLoadAddr,code,ram} a =
    if a >= codeLoadAddr
    then drop (a `minusAddr` codeLoadAddr) code
    else map (\n -> Map.findWithDefault 0x0 (a `addAddr` n) ram) [0..]

memStore :: State -> Addr -> Byte -> State
memStore mem@State{codeLoadAddr,ram} a b =
    if a >= codeLoadAddr
    then error "memStore,ROM!"
    else mem { ram = Map.insert a b ram }
