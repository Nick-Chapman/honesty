
module Honesty.Six502.Emu(
    cpuInstruction,
    triggerNMI,
    ) where

import Control.Monad (ap,liftM)
import Control.Monad.State(runStateT)
import Data.Bits
import Data.Set as Set

import Honesty.Addr
import Honesty.Byte
import Honesty.Nes as Nes
import Honesty.Six502.Cpu as Cpu
import Honesty.Six502.Cycles
import Honesty.Six502.Decode (decode1,opSize)
import Honesty.Six502.Operations
import qualified Honesty.Controller as Controller
import qualified Honesty.NesRam as NesRam
import qualified Honesty.PRG as PRG
import qualified Honesty.Six502.MM as MM
import qualified Honesty.Six502.Mem as Mem

type Buttons = Set Controller.Button

cpuInstruction :: Nes.RamRom -> PRG.ROM -> Buttons -> Nes.State -> NesRam.Effect (Nes.State,Cycles)
cpuInstruction Nes.RamRom{chr} prg2 buttons ns@Nes.State{cpu,cc} = do
    let opPrg1 = Nothing-- TODO
    let mm_eff = Mem.inter (opPrg1,prg2) (six_stepInstruction cpu)
    do
        ((cpu',cycles),ns') <- runStateT (MM.inter cc chr buttons mm_eff) ns
        let ns'' = ns' { cpu = cpu', cc = cc+cycles }
        return (ns'',cycles)

triggerNMI :: Nes.RamRom -> PRG.ROM -> Nes.State -> NesRam.Effect Nes.State
triggerNMI Nes.RamRom{chr} prg2 ns@Nes.State{cpu,cc} = do
    let opPrg1 = Nothing-- TODO
    let mm_eff = Mem.inter (opPrg1,prg2) (six_triggerNMI cpu)
    let buttons = Set.empty -- ok?
    do
        (cpu',ns') <- runStateT (MM.inter cc chr buttons mm_eff) ns
        return ns' { cpu = cpu' }


six_triggerNMI :: Cpu.State -> Mem.Effect Cpu.State
six_triggerNMI cpu = do
    (cpu,()) <- interpret nmi cpu
    return cpu
    where
        nmi :: Act ()
        nmi = do
            pc <- PC
            status <- Status
            pushStackA pc
            pushStack status
            lo <- ReadMem 0xfffa
            hi <- ReadMem 0xfffb
            SetPC $ addrOfHiLo hi lo

six_stepInstruction :: Cpu.State -> Mem.Effect (Cpu.State,Cycles)
six_stepInstruction = interpret fetchDecodeExec

fetchDecodeExec :: Act Cycles
fetchDecodeExec = do
    pc <- PC
    bytes <- ReadsMem pc
    let op = decode1 bytes
    SetPC (pc `addAddr` opSize op)
    action pc op

extraReadModifyWriteOpCycles :: Mode -> Cycles
extraReadModifyWriteOpCycles = \case
    ZeroPage -> 2
    ZeroPageX -> 2
    Absolute -> 2
    AbsoluteX -> 3
    _ -> 0

nopAbsoluteX :: Addr -> Act Cycles
nopAbsoluteX a = do
    (_,pageCrossed) <- absoluteX a
    cycles (if pageCrossed then 5 else 4)

action :: Addr -> Op -> Act Cycles
action pc = \case

    Op NOP_04 _ _ -> cycles 3
    Op NOP_44 _ _ -> cycles 3
    Op NOP_64 _ _ -> cycles 3

    Op NOP_0C _ _ -> cycles 4

    Op NOP_14 _ _ -> cycles 4
    Op NOP_34 _ _ -> cycles 4
    Op NOP_54 _ _ -> cycles 4
    Op NOP_74 _ _ -> cycles 4
    Op NOP_D4 _ _ -> cycles 4
    Op NOP_F4 _ _ -> cycles 4

    Op NOP_1A _ _ -> cycles 2
    Op NOP_3A _ _ -> cycles 2
    Op NOP_5A _ _ -> cycles 2
    Op NOP_7A _ _ -> cycles 2
    Op NOP_DA _ _ -> cycles 2
    Op NOP_FA _ _ -> cycles 2

    Op NOP_80 _ _ -> cycles 2

    Op NOP_1C AbsoluteX (ArgAddr a) -> nopAbsoluteX a
    Op NOP_3C AbsoluteX (ArgAddr a) -> nopAbsoluteX a
    Op NOP_5C AbsoluteX (ArgAddr a) -> nopAbsoluteX a
    Op NOP_7C AbsoluteX (ArgAddr a) -> nopAbsoluteX a
    Op NOP_DC AbsoluteX (ArgAddr a) -> nopAbsoluteX a
    Op NOP_FC AbsoluteX (ArgAddr a) -> nopAbsoluteX a

    Op NOP Implied ArgNull -> cycles 2

    Op SEC Implied ArgNull -> do SetFlag FlagCarry; cycles 2
    Op SEI Implied ArgNull -> do SetFlag FlagInterruptDisable; cycles 2
    Op SED Implied ArgNull -> do SetFlag FlagDecimal; cycles 2

    Op CLC Implied ArgNull -> do ClearFlag FlagCarry; cycles 2
    Op CLD Implied ArgNull -> do ClearFlag FlagDecimal; cycles 2
    Op CLV Implied ArgNull -> do ClearFlag FlagOverflow; cycles 2

    Op BCS Relative (ArgByte b) -> TestFlag FlagCarry >>= branch b
    Op BEQ Relative (ArgByte b) -> TestFlag FlagZero  >>= branch b
    Op BVS Relative (ArgByte b) -> TestFlag FlagOverflow  >>= branch b
    Op BMI Relative (ArgByte b) -> TestFlag FlagNegative  >>= branch b

    Op BCC Relative (ArgByte b) -> TestFlag FlagCarry >>= (branch b . not)
    Op BNE Relative (ArgByte b) -> TestFlag FlagZero  >>= (branch b . not)
    Op BVC Relative (ArgByte b) -> TestFlag FlagOverflow  >>= (branch b . not)
    Op BPL Relative (ArgByte b) -> TestFlag FlagNegative  >>= (branch b . not)

    Op ADC mode arg -> do
        in1 <- A
        (in2,n) <- load pc mode arg
        addWithCarry in1 in2
        return n

    Op SBC mode arg -> do
        in1 <- A
        (in2,n) <- load pc mode arg
        -- SBC takes the ones complement of the second value and then performs an ADC.
        addWithCarry in1 (complement in2)
        return n

    Op SBC_extra Immediate (ArgByte in2) -> do
        in1 <- A
        addWithCarry in1 (complement in2)
        return 2

    Op ASL mode arg -> do
        (v,n) <- load pc mode arg
        let v' = shiftL v 1
        updateFlag (testBit v 7) FlagCarry
        _ <- store pc mode arg v'
        updateNZ v'
        return $ extraReadModifyWriteOpCycles mode + n

    Op ROL mode arg -> do
        (v,n) <- load pc mode arg
        cin <- TestFlag FlagCarry
        updateFlag (testBit v 7) FlagCarry
        let v' = (if cin then setBit else clearBit) (shiftL v 1) 0
        _ <- store pc mode arg v'
        updateNZ v'
        return $ extraReadModifyWriteOpCycles mode + n

    Op LSR mode arg -> do
        (v,n) <- load pc mode arg
        let v' = shiftR v 1
        updateFlag (testBit v 0) FlagCarry
        _ <- store pc mode arg v'
        updateNZ v'
        return $ extraReadModifyWriteOpCycles mode + n

    Op ROR mode arg -> do
        (v,n) <- load pc mode arg
        cin <- TestFlag FlagCarry
        updateFlag (testBit v 0) FlagCarry
        let v' = (if cin then setBit else clearBit) (shiftR v 1) 7
        _ <- store pc mode arg v'
        updateNZ v'
        return $ extraReadModifyWriteOpCycles mode + n

    Op INC mode arg -> do
        (v,n) <- load pc mode arg
        let v' = v + 1
        _ <- store pc mode arg v'
        updateNZ v'
        return $ extraReadModifyWriteOpCycles mode + n

    Op DEC mode arg -> do
        (v,n) <- load pc mode arg
        let v' = v - 1
        _ <- store pc mode arg v'
        updateNZ v'
        return $ extraReadModifyWriteOpCycles mode + n

    Op INX Implied ArgNull -> do
        v <- X
        let v' = v + 1
        SetX v'
        updateNZ v'
        cycles 2

    Op INY Implied ArgNull -> do
        v <- Y
        let v' = v + 1
        SetY v'
        updateNZ v'
        cycles 2

    Op DEX Implied ArgNull -> do
        v <- X
        let v' = v - 1
        SetX v'
        updateNZ v'
        cycles 2

    Op DEY Implied ArgNull -> do
        v <- Y
        let v' = v - 1
        SetY v'
        updateNZ v'
        cycles 2

    Op AND mode arg -> do
        acc <- A
        (v,n) <- load pc mode arg
        let v' = acc .&. v
        SetA v'
        updateNZ v'
        return n

    Op ORA mode arg -> do
        acc <- A
        (v,n) <- load pc mode arg
        let v' = acc .|. v
        SetA v'
        updateNZ v'
        return n

    Op EOR mode arg -> do
        acc <- A
        (v,n) <- load pc mode arg
        let v' = acc `xor` v
        SetA v'
        updateNZ v'
        return n

    Op BIT mode arg -> do
        mask <- A
        (v,n) <- load pc mode arg
        updateFlag (v .&. mask == 0x0) FlagZero
        updateFlag (testBit v 6) FlagOverflow
        updateFlag (testBit v 7) FlagNegative
        return n

    Op JMP Absolute (ArgAddr a) -> do
        SetPC a
        cycles 3

    Op JMP Indirect (ArgAddr a) -> do

        --let a' = a + 1

        -- An original 6502 has does not correctly fetch the target
        -- address if the indirect vector falls on a page boundary
        -- (e.g. $xxFF where xx is any value from $00 to $FF). In this
        -- case fetches the LSB from $xxFF as expected but takes the
        -- MSB from $xx00.

        let a' = addrOfHiLo hi (lo + 1) where (hi,lo) = addrToHiLo a
        lo <- ReadMem a
        hi <- ReadMem a'
        let addr = addrOfHiLo hi lo
        SetPC addr
        cycles 5

    Op LAX mode arg -> do
        (b,n) <- load pc mode arg
        SetA b
        SetX b
        updateNZ b
        return n

    Op LDA mode arg -> do
        (b,n) <- load pc mode arg
        SetA b
        updateNZ b
        return n

    Op LDX mode arg -> do
        (b,n) <- load pc mode arg
        SetX b
        updateNZ b
        return n

    Op LDY mode arg -> do
        (b,n) <- load pc mode arg
        SetY b
        updateNZ b
        return n

    Op SAX mode arg -> do
        v1 <- A
        v2 <- X
        store pc mode arg (v1 .&. v2)

    Op STA mode arg -> do
        v <- A
        store pc mode arg v

    Op STX mode arg -> do
        v <- X
        store pc mode arg v

    Op STY mode arg -> do
        v <- Y
        store pc mode arg v

    Op TAX Implied ArgNull -> transfer A SetX
    Op TAY Implied ArgNull -> transfer A SetY
    Op TYA Implied ArgNull -> transfer Y SetA
    Op TXA Implied ArgNull -> transfer X SetA
    Op TSX Implied ArgNull -> transfer SP SetX
    Op TXS Implied ArgNull -> transferNoFlags X SetSP

    Op JSR Absolute (ArgAddr a) -> do
        here <- PC
        pushStackA (here `addAddr` (-1))
        SetPC a
        cycles 6

    Op RTS Implied ArgNull -> do
        target <- popStackA
        SetPC (target `addAddr` 1)
        cycles 6

    Op RTI Implied ArgNull -> do
        status <- popStack
        pc' <- popStackA
        SetStatus status
        SetPC pc'
        cycles 6

    Op PHP Implied ArgNull -> do
        byte <- Status
        pushStack (byte .|. 0x30) -- The B flag!
        cycles 3

    Op PHA Implied ArgNull -> do
        byte <- A
        pushStack byte
        cycles 3

    Op PLA Implied ArgNull -> do
        v <- popStack
        SetA v
        updateNZ v
        cycles 4

    Op PLP Implied ArgNull -> do
        v <- popStack
        SetStatus v
        cycles 4

    Op CMP mode arg -> do
        a <- A
        (b,n) <- load pc mode arg
        compareBytes a b
        return n

    Op CPX mode arg -> do
        a <- X
        (b,n) <- load pc mode arg
        compareBytes a b
        return n

    Op CPY mode arg -> do
        a <- Y
        (b,n) <- load pc mode arg
        compareBytes a b
        return n

    op -> error $ unwords ["action:",show pc,show op]

load :: Addr -> Mode -> Arg -> Act (Byte,Cycles)
load pc mode arg = case (mode,arg) of

    (Immediate, ArgByte v) -> do
        return (v,2)

    (Accumulator, ArgNull) -> do
        v <- A
        return (v,2)

    (ZeroPage, ArgByte b) -> do
        let a = zeroPageAddr b
        v <- ReadMem a
        return (v,3)

    (ZeroPageX, ArgByte b) -> do
        a <- zeroPageX b
        v <- ReadMem a
        return (v,4)

    (ZeroPageY, ArgByte b) -> do
        a <- zeroPageY b
        v <- ReadMem a
        return (v,4)

    (Absolute, ArgAddr a) -> do
        v <- ReadMem a
        return (v,4)

    (AbsoluteX, ArgAddr a) -> do
        (ea,pageCrossed) <- absoluteX a
        v <- ReadMem ea
        return (v,if pageCrossed then 5 else 4)

    (AbsoluteY, ArgAddr a) -> do
        (ea,pageCrossed) <- absoluteY a
        v <- ReadMem ea
        return (v,if pageCrossed then 5 else 4)

    (IndexedIndirect, ArgByte b) -> do
        a <- indexedIndirect b
        v <- ReadMem a
        return (v,6)

    (IndirectIndexed, ArgByte b) -> do
        (a,pageCrossed) <- indirectIndexed b
        v <- ReadMem a
        return (v,if pageCrossed then 6 else 5)

    x -> error $ "load: " <> show (pc,x)

store :: Addr -> Mode -> Arg -> Byte -> Act Cycles
store pc mode arg v = case (mode,arg) of

    (Accumulator, ArgNull) -> do
        SetA v
        return 0

    (ZeroPage, ArgByte b) -> do
        let a = zeroPageAddr b
        StoreMem a v
        cycles 3

    (ZeroPageX, ArgByte b) -> do
        a <- zeroPageX b
        StoreMem a v
        cycles 4

    (ZeroPageY, ArgByte b) -> do
        a <- zeroPageY b
        StoreMem a v
        cycles 4

    (Absolute, ArgAddr a) -> do
        StoreMem a v
        cycles 4

    (AbsoluteX, ArgAddr a) -> do
        (ea,_) <- absoluteX a
        StoreMem ea v
        return 5

    (AbsoluteY, ArgAddr a) -> do
        (ea,_) <- absoluteY a
        StoreMem ea v
        return 5

    (IndexedIndirect, ArgByte b) -> do
        a <- indexedIndirect b
        StoreMem a v
        return 6

    (IndirectIndexed, ArgByte b) -> do
        (a,_) <- indirectIndexed b
        StoreMem a v
        return 6

    x -> error $ "store: " <> show (pc,x)


zeroPageX :: Byte -> Act Addr
zeroPageX b = do
    x <- X
    return $ zeroPageAddr (b + x)

zeroPageY :: Byte -> Act Addr
zeroPageY b = do
    y <- Y
    return $ zeroPageAddr (b + y)

absoluteX :: Addr -> Act (Addr, Bool)
absoluteX a = do
    x <- X
    return $ indexAddress a x

absoluteY :: Addr -> Act (Addr, Bool)
absoluteY a = do
    y <- Y
    return $ indexAddress a y

indexedIndirect :: Byte -> Act Addr
indexedIndirect b = do
    x <- X
    readZeroPageAddr (b + x)

indirectIndexed :: Byte -> Act (Addr,Bool)
indirectIndexed b = do
    y <- Y
    a <- readZeroPageAddr b
    return $ indexAddress a y


readZeroPageAddr :: Byte -> Act Addr
readZeroPageAddr b = do
    lo <- ReadMem (zeroPageAddr b)
    hi <- ReadMem (zeroPageAddr (b + 1)) -- increment *then* wrap
    return $ addrOfHiLo hi lo

indexAddress :: Addr -> Byte -> (Addr,Bool)
indexAddress a b = do
    let (hi,lo) = addrToHiLo a
    let (lo',pageCrossed) = adc False lo b
    let ea = addrOfHiLo (if pageCrossed then hi+1 else hi) lo'
    (ea,pageCrossed)

transfer :: Act Byte  -> (Byte -> Act ()) -> Act Cycles
transfer from into = do
    v <- from
    into v
    updateNZ v
    cycles 2

transferNoFlags :: Act Byte  -> (Byte -> Act ()) -> Act Cycles
transferNoFlags from into = do
    v <- from
    into v
    cycles 2

compareBytes :: Byte -> Byte -> Act ()
compareBytes a b = do
    updateFlag (a == b) FlagZero
    updateFlag (a >= b) FlagCarry
    updateFlag (testBit (a - b) 7) FlagNegative

updateNZ :: Byte -> Act ()
updateNZ b = do
    updateFlag (b == 0x0) FlagZero
    updateFlag (testBit b 7) FlagNegative

updateFlag :: Bool -> Flag -> Act ()
updateFlag cond = if cond then SetFlag else ClearFlag

branch :: Byte -> Bool-> Act Cycles
branch offset cond = do
    if cond
        then do jumpRel $ byteToSigned offset; cycles 3 -- +1 more if to a new page
        else cycles 2

jumpRel :: Int -> Act ()
jumpRel offset = do
    pc <- PC
    SetPC (pc `addAddr` offset)

cycles :: Int -> Act Cycles
cycles n = return $ fromIntegral n

pushStackA :: Addr -> Act ()
pushStackA addr = do
    let (hi,lo) = addrToHiLo addr
    pushStack hi
    pushStack lo

popStackA :: Act Addr
popStackA = do
    lo <- popStack
    hi <- popStack
    return $ addrOfHiLo hi lo

pushStack :: Byte -> Act ()
pushStack byte = do
    top <- SP
    StoreMem (page1Addr top) byte
    SetSP (top - 1)

popStack :: Act Byte
popStack = do
    top <- SP
    let top' = top + 1
    SetSP top'
    ReadMem (page1Addr top')

addWithCarry :: Byte -> Byte -> Act ()
addWithCarry in1 in2 = do
    cin <- TestFlag FlagCarry
    let (res,cout) = adc cin in1 in2
    SetA res
    updateFlag cout FlagCarry
    updateFlag (res == 0x0) FlagZero
    let signRes = testBit res 7
    -- The overflow flag is set if the sign-bit of the result does match either of the args.
    let signOverflow = (signRes /= signIn1) && (signRes /= signIn2)
            where signIn1 = testBit in1 7
                  signIn2 = testBit in2 7
    updateFlag signOverflow FlagOverflow
    updateFlag signRes FlagNegative

data Flag
    = FlagCarry
    | FlagZero
    | FlagInterruptDisable
    | FlagDecimal
    | FlagOverflow
    | FlagNegative

bitNumOfFlag :: Flag -> Int
bitNumOfFlag = \case
    FlagCarry -> 0
    FlagZero -> 1
    FlagInterruptDisable -> 2
    FlagDecimal -> 3
    FlagOverflow -> 6
    FlagNegative -> 7

data Act a where
    Ret :: a -> Act a
    Bind :: Act a -> (a -> Act b) -> Act b
    ReadMem :: Addr -> Act Byte
    ReadsMem :: Addr -> Act [Byte]
    StoreMem :: Addr -> Byte -> Act ()

    PC :: Act Addr
    A :: Act Byte
    X :: Act Byte
    Y :: Act Byte
    SP :: Act Byte
    Status :: Act Byte

    SetPC :: Addr -> Act ()
    SetA :: Byte -> Act ()
    SetX :: Byte -> Act ()
    SetY :: Byte -> Act ()
    SetSP :: Byte -> Act ()
    SetStatus :: Byte -> Act ()

    TestFlag :: Flag -> Act Bool
    SetFlag :: Flag -> Act ()
    ClearFlag :: Flag -> Act ()

instance Functor Act where fmap = liftM
instance Applicative Act where pure = return; (<*>) = ap
instance Monad Act where return = Ret; (>>=) = Bind

interpret :: Act a -> Cpu.State -> Mem.Effect (Cpu.State,a)
interpret act cpu = do
    let Cpu.State{pc,accumulator,xreg,yreg,sp,status} = cpu
    case act of
        Ret a -> nochange a
        Bind m f -> do (cpu',a) <- interpret m cpu; interpret (f a) cpu'

        ReadMem addr -> do byte <- Mem.Read addr; return (cpu,byte)
        ReadsMem addr -> do bytes <- Mem.reads addr; return (cpu,bytes)
        StoreMem addr byte -> do Mem.Write addr byte; return (cpu,())

        PC -> nochange pc
        A -> nochange accumulator
        X -> nochange xreg
        Y -> nochange yreg
        SP -> nochange sp
        Status -> nochange status

        SetPC addr -> return (cpu {pc=addr}, ())
        SetA byte -> return (cpu {accumulator=byte}, ())
        SetX byte -> return (cpu {xreg=byte}, ())
        SetY byte -> return (cpu {yreg=byte}, ())
        SetSP byte -> return (cpu {sp=byte}, ())

        SetStatus byte -> do
            let byte' = (byte .&. 0xCF) .|. 0x20 -- bits 5,4 always set to 1,0
            return (cpu {status=byte'}, ())

        TestFlag flag -> nochange $ testBit status (bitNumOfFlag flag)
        SetFlag flag -> return (cpu { status = setBit status $ bitNumOfFlag flag }, ())
        ClearFlag flag -> return (cpu { status = clearBit status $ bitNumOfFlag flag }, ())

    where
        nochange :: a -> Mem.Effect (Cpu.State,a)
        nochange x = return (cpu,x)
