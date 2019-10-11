
module Honesty.TraceCpu(printRun) where

import Honesty.Nes as Nes
import qualified Honesty.Emu as Emu
import qualified Honesty.Sim as Sim

printRun :: Int -> String -> IO ()
printRun n path = do
    (rr,pc0) <- rr0pc0 path
    let ns = state0 pc0
    let step = Emu.neverStopping
    let eff = Emu.interpret rr ns step
    Sim.trace n rr eff
