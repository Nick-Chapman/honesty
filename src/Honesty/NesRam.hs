
module Honesty.NesRam(
    Effect(..),
    MState, newMState,
    inter,
    ) where

import Prelude hiding (init,read)
import Control.Monad (ap,liftM)

import qualified Honesty.Ram2k as Ram2k

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
    Ret :: a -> Effect a
    Bind :: Effect a -> (a -> Effect b) -> Effect b
    InVram :: Ram2k.Effect a -> Effect a
    InWram :: Ram2k.Effect a -> Effect a
    EmbedIO :: IO a -> Effect a

data MState = MState
    { vram :: Ram2k.MState
    , wram :: Ram2k.MState }

newMState :: IO MState
newMState = do
    vram <- Ram2k.newMState "vram"
    wram <- Ram2k.newMState "wram"
    return $ MState {vram,wram}

inter :: MState -> Effect a -> IO a
inter MState{vram,wram} = loop where
  loop :: Effect a -> IO a
  loop = \case
    Ret x -> return x
    Bind e f -> do v <- loop e; loop (f v)
    InVram e -> Ram2k.interIO vram e
    InWram e -> Ram2k.interIO wram e
    EmbedIO io -> io
