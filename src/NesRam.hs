
module NesRam(
    Effect(..),
    MState, newMState,
    inter,
    ) where

import Prelude hiding (init,read)
import Control.Monad (ap,liftM)

import qualified Ram2k

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    InVram :: Ram2k.Effect a -> Effect a
    InWram :: Ram2k.Effect a -> Effect a

data MState = MState
    { vram :: Ram2k.MState
    , wram :: Ram2k.MState }

newMState :: IO MState
newMState = do
    vram <- Ram2k.newMState "vram"
    wram <- Ram2k.newMState "wram"
    return $ NesRam.MState {vram,wram}

inter :: MState -> Effect a -> IO a
inter m@MState{vram,wram} = \case
    Ret x -> return x
    Bind e f -> do v <- inter m e; inter m (f v)
    InVram e -> Ram2k.interIO vram e
    InWram e -> Ram2k.interIO wram e
