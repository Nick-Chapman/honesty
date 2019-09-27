
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

inter :: Pressed -> State -> Effect a -> (a,State)
inter pressed state = \case
    Strobe True -> ((), Strobing)
    Strobe False -> ((), Sampled $ map (`elem` pressed) [A .. Right])

    Read -> case state of
        Strobing -> (bool2byte $ A `elem` pressed, Strobing)
        Sampled bools -> case bools of
            [] -> (1, Sampled [])
            b:bs' -> (bool2byte b, Sampled bs')

bool2byte :: Bool -> Byte
bool2byte b = if b then 1 else 0
