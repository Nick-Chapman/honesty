
module Honesty.TraceCpu(printRun) where

import Data.Set as Set
import Control.Monad.State(runStateT)

import Honesty.Addr
import Honesty.Byte
import Honesty.Nes as Nes
import Honesty.Six502.Disassembler(ljust,displayOpLine)
import qualified Honesty.Emu as Emu
import qualified Honesty.NesRam as NesRam
import qualified Honesty.Sim as Sim
import qualified Honesty.Six502.Cpu as Six502.Cpu
import qualified Honesty.Six502.Decode as Six502.Decode
import qualified Honesty.Six502.MM as MM
import qualified Honesty.Six502.Mem as Six502.Mem

printRun :: Int -> String -> IO ()
printRun n path = do
    (rr,pc0) <- rr0pc0 path
    let ns = state0 pc0
    let step = Emu.neverStopping
    let eff = Emu.interpret rr ns step
    Sim.trace n (printNS rr) eff

printNS :: Nes.RamRom -> Nes.State -> IO ()
printNS rr ns@Nes.State{cpu,cc} = do
    let Six502.Cpu.State{Six502.Cpu.pc} = cpu
    bytes <- readFromAddr ns rr pc
    let op = Six502.Decode.decode1 bytes
    let col = 48
    let s = ljust col (displayOpLine pc op) <> show cpu  <> " " <> show cc
    putStrLn s

readFromAddr :: Nes.State -> Nes.RamRom -> Addr -> IO [Byte]
readFromAddr ns@Nes.State{cc} Nes.RamRom{prg,chr,ram} pc = do
    let opPrg1 = Nothing
    -- TODO: move seq of 3 interpreters into CPU/Emu code (and share)
    let mem_eff = Six502.Mem.reads pc
    let mm_eff = Six502.Mem.inter (opPrg1,prg) mem_eff
    let buttons = Set.empty
    (bytes,_) <- NesRam.inter ram $ runStateT (MM.inter cc chr buttons mm_eff) ns
    return bytes
