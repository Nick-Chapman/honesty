
module Honesty.TraceCpu(printRun,printRunBLA) where

import Honesty.Nes as Nes (rr0pc0,state0)
import qualified Honesty.Emu as Emu (neverStopping,interpret)
import qualified Honesty.Sim as Sim (trace,traceBLA)

printRun :: Int -> String -> IO ()
printRun n path = do
    (rr,pc0) <- rr0pc0 path
    let ns = state0 pc0
    let step = Emu.neverStopping
    let debug = False
    let eff = Emu.interpret debug rr ns step
    Sim.trace n rr eff

printRunBLA :: Int -> String -> IO () -- for trace-log comparison against hnes of blargg tests
printRunBLA n path = do
    (rr,pc0) <- rr0pc0 path
    let ns = state0 pc0
    let step = Emu.neverStopping
    let debug = False
    let eff = Emu.interpret debug rr ns step
    Sim.traceBLA n rr eff
