
module NesRam(
    Effect(..),
    State,
    run,

    Trans, runTrans, interpretST,
    --Trans', runTrans', interpretST',

    ) where -- 2x 2k ram (wram/vram)

import Prelude hiding (init,read)
import Control.Monad (ap,liftM)


import Control.Monad.ST
--import Control.Monad.Trans.ST
--import Control.Monad.Trans.Class

import Data.Array.MArray hiding (inRange)
import Data.Array.ST hiding (inRange)


import Six502.Values(Byte)

import qualified Ram2k



instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    Wram :: Ram2k.Effect a -> Effect a
    Vram :: Ram2k.Effect a -> Effect a

data State = State { vram :: Ram2k.State, wram :: Ram2k.State }

run :: State -> Effect a -> (State,a)
run state = \case
    Ret x -> (state,x)
    Bind e f -> let (state',a) = run state e in run state' (f a)
    Vram e -> do
        let State{vram=v} = state
        let (vram,x) = Ram2k.run v e
        (state { vram }, x)
    Wram e -> do
        let State{wram=w} = state
        let (wram,x) = Ram2k.run w e
        (state { wram }, x)



type Arr x = STArray x Int Byte

type Trans x a = ST x a

runTrans :: (forall x. ST x a) -> a
runTrans = runST

interpretST :: forall a x. Effect a -> Trans x a
interpretST e = do
    vram <- newArray (0,Ram2k.size-1) 0
    wram <- newArray (0,Ram2k.size-1) 0
    inter2 (vram,wram) e

inter2 :: (Arr x,Arr x) -> Effect a -> ST x a
inter2 s@(vram,wram) = \case
    Ret x -> return x
    Bind e f -> do v <- inter2 s e; inter2 s (f v)
    Vram e -> inter1 vram e
    Wram e -> inter1 wram e

inter1 :: Arr x -> Ram2k.Effect a -> ST x a
inter1 arr = \case
    Ram2k.Ret x -> return x
    Ram2k.Bind e f -> do v <- inter1 arr e; inter1 arr (f v)
    Ram2k.Read a -> readArray arr a
    Ram2k.Write a b -> writeArray arr a b


{-
-- coded by composing 2 copies of the interpreter for Ram2k

type Trans' x y a = STT x (ST y) a

runTrans' :: (forall x y. Trans' x y a) -> a
runTrans' tr = runST (runSTT tr)

interpretST' :: Effect a -> Trans' x y a
interpretST' = \case
    Ret x -> return x
    Bind e f -> do v <- interpretST' e; interpretST' (f v)
    Vram e -> Ram2k.interpretSTT e
    Wram e -> lift $ Ram2k.interpretST e
-}
