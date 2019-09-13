
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

    Op ADC Immediate (ArgByte in2) -> do
        in1 <- A
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
        cycles 2

    Op SBC Immediate (ArgByte in2) -> do
        -- SBC takes the ones complement of the second value and then performs an ADC.
        action $ Op ADC Immediate (ArgByte (complement in2))

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

    Op AND Immediate (ArgByte b) -> do
        acc <- A
        let v = acc .&. b
        SetA v
        updateNZ v
        cycles 2

    Op ORA Immediate (ArgByte b) -> do
        acc <- A
        let v = acc .|. b
        SetA v
        updateNZ v
        cycles 2

    Op EOR Immediate (ArgByte b) -> do
        acc <- A
        let v = acc `xor` b
        SetA v
        updateNZ v
        cycles 2

    Op BIT ZeroPage (ArgByte b) -> do
        mask <- A
        v <- ReadMem (zeroPageAddr b)
        updateFlag (v .&. mask == 0x0) FlagZero
        updateFlag (testBit v 6) FlagOverflow
        updateFlag (testBit v 7) FlagNegative
        cycles 3

    Op JMP Absolute (ArgAddr a) -> do
        SetPC a
        cycles 3

    Op LDA mode arg -> do
        (b,n) <- load mode arg
        SetA b
        updateNZ b
        return n

    Op LDX mode arg -> do
        (b,n) <- load mode arg
        SetX b
        updateNZ b
        return n

    Op LDY Immediate (ArgByte b) -> do
        SetY b
        updateNZ b
        cycles 2

    Op STA ZeroPage (ArgByte b) -> do
        v <- A;
        StoreMem (zeroPageAddr b) v
        cycles 3

    Op STX ZeroPage (ArgByte b) -> do
        v <- X;
        StoreMem (zeroPageAddr b) v
        cycles 3

    Op STX Absolute (ArgAddr addr) -> do
        v <- X;
        StoreMem addr v
        cycles 4

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

    Op CMP Immediate (ArgByte b) -> do
        a <- A
        compareBytes a b
        cycles 2

    Op CPX Immediate (ArgByte b) -> do
        x <- X
        compareBytes x b
        cycles 2

    Op CPY Immediate (ArgByte b) -> do
        y <- Y
        compareBytes y b
        cycles 2

    op -> error $ "action: " <> show op


load :: Mode -> Arg -> Act (Byte,Cycles)
load mode arg = case (mode,arg) of
    (Immediate, ArgByte b) -> return (b,2)
    (Absolute, ArgAddr a) -> do b <- ReadMem a; return (b,4)
    x -> error $ show x

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