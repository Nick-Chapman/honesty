
module ArchIdeas where

--newtype Identity a = Identity a
--newtype StateT s m a = StateT { runStateT :: s -> m (a,a) }
--newtype State s a = State { runState :: s -> (a,s) }

import Control.Monad.Identity
import Control.Monad.State

import Control.Monad (ap,liftM)

data EitherEff m1 m2 a = LeftEff (m1 a) | RightEff (m2 a)

runEitherEff :: (m1 a -> b) -> (m2 a -> b) -> EitherEff m1 m2 a -> b
runEitherEff = undefined

cpuAct :: CpuAct () -- needs to be a monad!
cpuAct = do
    fetchDecodeExec -- repeated xN
    --triggerNMI

cpuState0 :: CpuState


data NesState = NesState CpuState ControllerState


--sysTop :: PrgRom -> RamT (StateT NesState RegsEff) ()
sysTop :: PrgRom -> NesState -> RegsEff NesState
sysTop prgRom (NesState cpuState controllerState) = do

    let cpuMemEff :: CpuMemEff ((),CpuState) = runStateT (iCpu cpuAct) cpuState
    let ee :: EitherEff ControllerEff RegsEff ((),CpuState) =
            runRamT (iCpuMem prgRom cpuMemEff)

    let iC :: ControllerEff ((),CpuState) -> RegsEff (CpuState,ControllerState) =
            \conEff -> do
                let (((),cpuState'),controllerState') = runState (iController conEff) controllerState
                return (cpuState',controllerState')

    let iR :: RegsEff ((),CpuState) -> RegsEff (CpuState,ControllerState) =
            \regEff -> do
                ((),cpuState') <- regEff
                return (cpuState',controllerState)

    --let regsEff :: RegsEff (CpuState,ControllerState) = runEitherEff iC iR ee
    do
        (cpuState',controllerState') <- runEitherEff iC iR ee -- regsEff
        return $ NesState cpuState' controllerState'

{-
fetchDecodeExec :: CpuAct ()
iCpu :: CpuAct a -> StateT CpuState CpuMemEff a
iCpuMem :: PrgRom -> CpuMemEff a -> RamT (EitherEff ControllerEff RegsEff) a
iController :: ControllerEff a -> State ControllerState a
-}


----------------------------------------------------------------------

fetchDecodeExec :: CpuAct ()
triggerNMI :: CpuAct ()

data CpuAct a

iCpu :: CpuAct a -> StateT CpuState CpuMemEff  a

data CpuState

data CpuMemEff a

iCpuMem :: PrgRom -> CpuMemEff a -> RamT (EitherEff ControllerEff RegsEff) a

data PrgRom

data ControllerEff a

iController :: ControllerEff a -> State ControllerState a

data ControllerState

ppuRender :: EitherEff RegsEff PpuMemEff Screen

data Screen

data RegsEff a where
    RegsEff_Ret :: a -> RegsEff a
    RegsEff_Bind :: RegsEff a -> (a -> RegsEff b) -> RegsEff b

instance Functor RegsEff where fmap = liftM
instance Applicative RegsEff where pure = return; (<*>) = ap
instance Monad RegsEff where return = RegsEff_Ret; (>>=) = RegsEff_Bind



iRegs :: RegsEff a -> StateT RegsState PpuMemEff a

data RegsState

data PpuMemEff a

iPpuMem :: ChrRom -> PpuMemEff a -> RamT Identity a

data ChrRom

newtype RamT m a = RamT { unRam :: StateT RamState m a } -- 2k ram (wram/vram)
    deriving (Functor,Applicative,Monad)

runRamT :: RamT m a -> m a

data RamState

----------------------------------------------------------------------
(fetchDecodeExec, triggerNMI, iCpu, iCpuMem, iController, ppuRender, iRegs, iPpuMem,
 cpuState0, runRamT)
    = undefined
