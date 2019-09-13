
module Six502.Emu(
    run,
    State,
    showState,
    ) where

import Control.Monad (ap,liftM)
import Data.Bits
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Six502.Types
import Six502.Decode
import Six502.Disassembler

run :: [Byte] -> [State]
run code = stepFrom (state0 code)
    where stepFrom s = s : stepFrom (step s)

data State = State
    { mem :: Mem
    , cpu :: Cpu
    , cc :: Cycles
    }

showState :: State -> String
showState state = do
    let State{mem,cpu,cc} = state
    let Cpu{pc} = cpu
    let bytes = memStarting mem pc
    let op = decode1 bytes
    let col = 48
    ljust col (displayOpLine pc op) <> show cpu <> " " <> show cc

state0 :: [Byte] -> State
state0 code = State
    { mem = mem0 code
    , cpu = cpu0
    , cc = 7 -- from nestest.log
    }

step :: State -> State
step s = do
    let State{mem,cpu,cc} = s
    let (mem',(cpu',n)) = runMem (interpret fetchDecodeExec cpu) mem
    s { mem = mem', cpu = cpu', cc = cc + n }


newtype Cycles = Cycles Int deriving (Num)

instance Show Cycles where
    show (Cycles n) = "CYC:" <> show n


codeLoadAddr :: Addr
codeLoadAddr = 0xC000

data Mem = Mem
    { code :: [Byte]
    , ram :: Map Addr Byte
    }

mem0 :: [Byte] -> Mem
mem0 code = Mem {code, ram = Map.empty}

memStarting :: Mem -> Addr -> [Byte]
memStarting Mem{code,ram} a =
    if a >= codeLoadAddr
    then drop (a `minusAddr` codeLoadAddr) code
    else map (\n -> Map.findWithDefault byte0 (a `addAddr` n) ram) [0..]

memUpdate :: Mem -> Addr -> Byte -> Mem
memUpdate mem@Mem{ram} a b =
    if a >= codeLoadAddr
    then error "memUpdate,ROM!"
    else mem { ram = Map.insert a b ram }

data MemEffect a where
    MemRet :: a -> MemEffect a
    MemBind :: MemEffect a -> (a -> MemEffect b) -> MemEffect b
    MemRead :: Addr -> MemEffect [Byte]
    MemStore :: Addr -> Byte -> MemEffect ()


instance Functor MemEffect where fmap = liftM
instance Applicative MemEffect where pure = return; (<*>) = ap
instance Monad MemEffect where return = MemRet; (>>=) = MemBind

runMem :: MemEffect a -> Mem -> (Mem,a)
runMem eff mem =
    case eff of
        MemRet a -> (mem,a)
        MemBind m f -> do
            let (mem',a) = runMem m mem
            runMem (f a) mem'
        MemRead addr -> (mem, memStarting mem addr)
        MemStore addr byte -> (memUpdate mem addr byte,())

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
    SetPC (pc `addAddr` sizeOp op)
    return $ op

action :: Op -> Act Cycles
action = \case

    Op NOP Implied RandNull -> cycles 2

    Op SEC Implied RandNull -> do SetFlag FlagCarry; cycles 2
    Op SEI Implied RandNull -> do SetFlag FlagInterruptDisable; cycles 2
    Op SED Implied RandNull -> do SetFlag FlagDecimal; cycles 2

    Op CLC Implied RandNull -> do ClearFlag FlagCarry; cycles 2
    Op CLD Implied RandNull -> do ClearFlag FlagDecimal; cycles 2
    Op CLV Implied RandNull -> do ClearFlag FlagOverflow; cycles 2

    Op BCS Relative (RandByte b) -> TestFlag FlagCarry >>= branch b
    Op BEQ Relative (RandByte b) -> TestFlag FlagZero  >>= branch b
    Op BVS Relative (RandByte b) -> TestFlag FlagOverflow  >>= branch b
    Op BMI Relative (RandByte b) -> TestFlag FlagNegative  >>= branch b

    Op BCC Relative (RandByte b) -> TestFlag FlagCarry >>= (branch b . not)
    Op BNE Relative (RandByte b) -> TestFlag FlagZero  >>= (branch b . not)
    Op BVC Relative (RandByte b) -> TestFlag FlagOverflow  >>= (branch b . not)
    Op BPL Relative (RandByte b) -> TestFlag FlagNegative  >>= (branch b . not)

    Op AND Immediate (RandByte b) -> do
        acc <- A
        let v = acc .&. b
        SetA v
        updateNZ v
        cycles 2

    Op ORA Immediate (RandByte b) -> do
        acc <- A
        let v = acc .|. b
        SetA v
        updateNZ v
        cycles 2

    Op EOR Immediate (RandByte b) -> do
        acc <- A
        let v = acc `xor` b
        SetA v
        updateNZ v
        cycles 2

    Op BIT ZeroPage (RandByte b) -> do
        mask <- A
        v <- ReadMem (zeroPageAddr b)
        updateNVZ (v .&. mask)
        cycles 3

    Op JMP Absolute (RandAddr a) -> do
        SetPC a
        cycles 3

    Op LDA Immediate (RandByte b) -> do
        SetA b
        updateNZ b
        cycles 2

    Op LDX Immediate (RandByte b) -> do
        SetX b
        updateNZ b
        cycles 2

    Op STA ZeroPage (RandByte b) -> do
        v <- A;
        StoreMem (zeroPageAddr b) v
        cycles 3

    Op STX ZeroPage (RandByte b) -> do
        v <- X;
        StoreMem (zeroPageAddr b) v
        cycles 3

    Op JSR Absolute (RandAddr a) -> do
        here <- PC
        pushStackA (here `addAddr` (-1))
        SetPC a
        cycles 6

    Op RTS Implied RandNull -> do
        target <- popStackA
        SetPC (target `addAddr` 1)
        cycles 6

    Op PHP Implied RandNull -> do
        byte <- Status
        pushStack (byte .|. 0x30) -- The B flag!
        cycles 3

    Op PHA Implied RandNull -> do
        byte <- A
        pushStack byte
        cycles 3

    Op PLA Implied RandNull -> do
        v <- popStack
        SetA v
        updateNZ v
        cycles 4

    Op PLP Implied RandNull -> do
        v <- popStack
        SetStatus v
        cycles 4

    Op CMP Immediate (RandByte b) -> do
        a <- A
        compareBytes a b
        cycles 2

    op -> error $ "action: " <> show op

compareBytes :: Byte -> Byte -> Act ()
compareBytes a b = do
    updateFlag (a == b) FlagZero
    updateFlag (a >= b) FlagCarry
    updateFlag (a < b) FlagNegative

updateNZ :: Byte -> Act ()
updateNZ b = do
    updateFlag (b == 0x0) FlagZero
    updateFlag (testBit b 7) FlagNegative

updateNVZ :: Byte -> Act ()
updateNVZ b = do
    updateFlag (b == 0x0) FlagZero
    updateFlag (testBit b 7) FlagNegative
    updateFlag (testBit b 6) FlagOverflow

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

pushStack :: Byte -> Act ()
pushStack byte = do
    top <- SP
    let top' = top `addByte` (-1)
    StoreMem (page1Addr top') byte
    SetSP top'

popStackA :: Act Addr
popStackA = do
    lo <- popStack
    hi <- popStack
    return $ addrOfHiLo hi lo

popStack :: Act Byte
popStack = do
    top <- SP
    SetSP (top `addByte` 1)
    ReadMem (page1Addr top)

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


interpret :: Act a -> Cpu -> MemEffect (Cpu,a)
interpret act cpu = do
    let Cpu{pc,accumulator,xreg,yreg,sp,status} = cpu
    case act of
        Ret a -> nochange a
        Bind m f -> do (cpu',a) <- interpret m cpu; interpret (f a) cpu'

        ReadMem addr -> do bytes <- MemRead addr; return (cpu,head bytes)
        ReadsMem addr -> do bytes <- MemRead addr; return (cpu,bytes)
        StoreMem addr byte -> do MemStore addr byte; return (cpu,())

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
        nochange :: a -> MemEffect (Cpu,a)
        nochange x = return (cpu,x)
