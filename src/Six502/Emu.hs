
module Six502.Emu(
    run,
    State,
    showState,
    ) where

import Control.Monad (ap,liftM)
import Data.Bits

import Six502.Values
import Six502.Operations
import Six502.Decode (decode1,opSize)
import Six502.Disassembler (ljust,displayOpLine)
import qualified Six502.Mem as Mem

codeLoadAddr :: Addr
codeLoadAddr = 0xC000

run :: [Byte] -> [State]
run code = stepFrom (state0 code)
    where stepFrom s = s : stepFrom (step s)

data State = State
    { mem :: Mem.State
    , cpu :: Cpu
    , cc :: Cycles
    }

showState :: State -> String
showState state = do
    let State{mem,cpu,cc} = state
    let Cpu{pc} = cpu
    let bytes = snd $ Mem.run (Mem.Reads pc) mem
    let op = decode1 bytes
    let col = 48
    ljust col (displayOpLine pc op) <> show cpu <> " " <> show cc

state0 :: [Byte] -> State
state0 code = State
    { mem = Mem.initializeWithCode codeLoadAddr code
    , cpu = cpu0
    , cc = 7 -- from nestest.log
    }

step :: State -> State
step s = do
    let State{mem,cpu,cc} = s
    let (mem',(cpu',n)) = Mem.run (interpret fetchDecodeExec cpu) mem
    s { mem = mem', cpu = cpu', cc = cc + n }

newtype Cycles = Cycles Int deriving (Num)

instance Show Cycles where show (Cycles n) = "CYC:" <> show n

data Cpu = Cpu
    { pc :: Addr
    , accumulator :: Byte
    , xreg :: Byte
    , yreg :: Byte
    , status :: Byte
    , sp :: Byte
    }

instance Show Cpu where
    show = \Cpu{accumulator,xreg,yreg,status,sp} -> unwords
        [ "A:" <> show accumulator
        , "X:" <> show xreg
        , "Y:" <> show yreg
        , "P:" <> show status
        , "SP:" <> show sp
        ]

cpu0 :: Cpu
cpu0 = Cpu
    { pc = codeLoadAddr
    , accumulator = byte0
    , xreg = byte0
    , yreg = byte0
    , sp = 0xFD
    , status = 0x24
    }

byte0 :: Byte
byte0 = 0x0

fetchDecodeExec :: Act Cycles
fetchDecodeExec = fetchDecode >>= action

fetchDecode :: Act Op
fetchDecode = do
    pc <- PC
    bytes <- ReadsMem pc
    let op = decode1 bytes
    SetPC (pc `addAddr` opSize op)
    return $ op

action :: Op -> Act Cycles
action = \case

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
        (in2,n) <- load mode arg 0
        addWithCarry in1 in2
        return n

    Op SBC mode arg -> do
        in1 <- A
        (in2,n) <- load mode arg 0
        -- SBC takes the ones complement of the second value and then performs an ADC.
        addWithCarry in1 (complement in2)
        return n

    Op ASL mode arg -> do
        (v,n) <- load mode arg 2
        let v' = shiftL v 1
        updateFlag (testBit v 7) FlagCarry
        _ <- store mode arg v'
        updateNZ v'
        return n

    Op ROL mode arg -> do
        (v,n) <- load mode arg 2
        cin <- TestFlag FlagCarry
        updateFlag (testBit v 7) FlagCarry
        let v' = (if cin then setBit else clearBit) (shiftL v 1) 0
        _ <- store mode arg v'
        updateNZ v'
        return n

    Op LSR mode arg -> do
        (v,n) <- load mode arg 2
        let v' = shiftR v 1
        updateFlag (testBit v 0) FlagCarry
        _ <- store mode arg v'
        updateNZ v'
        return n

    Op ROR mode arg -> do
        (v,n) <- load mode arg 2
        cin <- TestFlag FlagCarry
        updateFlag (testBit v 0) FlagCarry
        let v' = (if cin then setBit else clearBit) (shiftR v 1) 7
        _ <- store mode arg v'
        updateNZ v'
        return n

    Op INC mode arg -> do
        (v,n) <- load mode arg 2
        let v' = v + 1
        _ <- store mode arg v'
        updateNZ v'
        return n

    Op DEC mode arg -> do
        (v,n) <- load mode arg 2
        let v' = v - 1
        _ <- store mode arg v'
        updateNZ v'
        return n

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
        (v,n) <- load mode arg 0
        let v' = acc .&. v
        SetA v'
        updateNZ v'
        return n

    Op ORA mode arg -> do
        acc <- A
        (v,n) <- load mode arg 0
        let v' = acc .|. v
        SetA v'
        updateNZ v'
        return n

    Op EOR mode arg -> do
        acc <- A
        (v,n) <- load mode arg 0
        let v' = acc `xor` v
        SetA v'
        updateNZ v'
        return n

    Op BIT mode arg -> do
        mask <- A
        (v,n) <- load mode arg 0
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

    Op LDA mode arg -> do
        (b,n) <- load mode arg 0
        SetA b
        updateNZ b
        return n

    Op LDX mode arg -> do
        (b,n) <- load mode arg 0
        SetX b
        updateNZ b
        return n

    Op LDY mode arg -> do
        (b,n) <- load mode arg 0
        SetY b
        updateNZ b
        return n

    Op STA mode arg -> do
        v <- A
        store mode arg v

    Op STX mode arg -> do
        v <- X
        store mode arg v

    Op STY mode arg -> do
        v <- Y
        store mode arg v

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
        pc <- popStackA
        SetStatus status
        SetPC pc
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
        (b,n) <- load mode arg 0
        compareBytes a b
        return n

    Op CPX mode arg -> do
        a <- X
        (b,n) <- load mode arg 0
        compareBytes a b
        return n

    Op CPY mode arg -> do
        a <- Y
        (b,n) <- load mode arg 0
        compareBytes a b
        return n

    op -> error $ "action: " <> show op

load :: Mode -> Arg -> Cycles -> Act (Byte,Cycles)
load mode arg c = case (mode,arg) of

    (Immediate, ArgByte v) -> do
        return (v,2)

    (Accumulator, ArgNull) -> do
        v <- A
        return (v,2)

    (ZeroPage, ArgByte b) -> do
        v <- ReadMem (zeroPageAddr b)
        return (v,3+c)

    (Absolute, ArgAddr a) -> do
        v <- ReadMem a
        return (v,4+c)

    (IndexedIndirect, ArgByte b) -> do
        a <- indexedIndirect b
        v <- ReadMem a
        return (v,6)

    (IndirectIndexed, ArgByte b) -> do
        (a,pageCrossed) <- indirectIndexed b
        v <- ReadMem a
        return (v,if pageCrossed then 6 else 5)

    x -> error $ "load: " <> show x

store :: Mode -> Arg -> Byte -> Act Cycles
store mode arg v = case (mode,arg) of

    (Accumulator, ArgNull) -> do
        SetA v
        return 0

    (ZeroPage, ArgByte b) -> do
        StoreMem (zeroPageAddr b) v
        cycles 3

    (Absolute, ArgAddr a) -> do
        StoreMem a v
        cycles 4

    (IndexedIndirect, ArgByte b) -> do
        a <- indexedIndirect b
        StoreMem a v
        return 6

    (IndirectIndexed, ArgByte b) -> do
        (a,_) <- indirectIndexed b
        StoreMem a v
        return 6

    x -> error $ "store: " <> show x

indexedIndirect :: Byte -> Act Addr
indexedIndirect b = do
    x <- X
    lo <- ReadMem (zeroPageAddr (b + x))
    hi <- ReadMem (zeroPageAddr (b + x + 1)) -- increment *then* wrap
    return $ addrOfHiLo hi lo

indirectIndexed :: Byte -> Act (Addr,Bool)
indirectIndexed b = do
    y <- Y
    lo <- ReadMem (zeroPageAddr b)
    hi <- ReadMem (zeroPageAddr (b + 1)) -- increment *then* wrap
    let (lo',pageCrossed) = adc False lo y
    let addr = addrOfHiLo (if pageCrossed then hi+1 else hi) lo'
    return (addr,pageCrossed)

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
cycles n = return $ Cycles n

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

interpret :: Act a -> Cpu -> Mem.Effect (Cpu,a)
interpret act cpu = do
    let Cpu{pc,accumulator,xreg,yreg,sp,status} = cpu
    case act of
        Ret a -> nochange a
        Bind m f -> do (cpu',a) <- interpret m cpu; interpret (f a) cpu'

        ReadMem addr -> do bytes <- Mem.Reads addr; return (cpu,head bytes)
        ReadsMem addr -> do bytes <- Mem.Reads addr; return (cpu,bytes)
        StoreMem addr byte -> do Mem.Store addr byte; return (cpu,())

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
        nochange :: a -> Mem.Effect (Cpu,a)
        nochange x = return (cpu,x)
