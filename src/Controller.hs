
module Controller( -- keyboard-mapping is in Sim
    Button(..),
    State, state0,
    Effect(..), inter,
    ) where

import Prelude hiding (Right)
import Data.Set(Set)
import Six502.Values

data State = Strobing | Sampled [Bool] deriving (Show)

state0 :: State
state0 = Sampled []

data Effect a where
    Read :: Effect Byte
    Strobe :: Bool -> Effect ()

data Button = A | B | Select | Start | Up | Down | Left | Right deriving (Eq,Ord,Enum,Show)

type Pressed = Set Button -- only J1

inter :: Pressed -> State -> Effect a -> IO (a,State)
inter pressed state = \case
    Strobe True -> return ((), Strobing)
    Strobe False -> return ((), Sampled $ map (`elem` pressed) [A .. Right])

    Read ->
        case state of
            Strobing -> do
                let res = bool2byte $ A `elem` pressed
                --print ("Con.Read.inter(Strobing)",state,res)
                return (res, Strobing)
            Sampled bools -> case bools of
                [] -> do
                    --print ("Con.Read.inter(Sampled[])",state)
                    return (1, Sampled [])
                b:bs' -> do
                    --print ("Con.Read.inter(Sampled)",state,b)
                    return (bool2byte b, Sampled bs')

bool2byte :: Bool -> Byte
bool2byte b = if b then 1 else 0
