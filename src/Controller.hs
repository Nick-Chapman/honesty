
module Controller( -- keyboard-mapping is in Sim
    Button(..), State,
    init,
    pressJ1, releaseJ1, -- only joy stick 1 for now
    strobe, read,
    ) where

import Prelude hiding (init,read,Right)
import Data.Set(Set)
import qualified Data.Set as Set

data Button = A | B | Select | Start | Up | Down | Left | Right deriving (Eq,Ord,Enum,Show)

data Mode = Strobing | Sampled [Bool] deriving (Show)

data State = State { mode :: Mode, pressed :: Set Button } deriving (Show)

init :: State
init = State { mode = Sampled [], pressed = Set.empty }

pressJ1 :: Button -> State -> State
pressJ1 button state@State{pressed} = state { pressed = Set.insert button pressed }

releaseJ1 :: Button -> State -> State
releaseJ1 button state@State{pressed} = state { pressed = Set.delete button pressed }

strobe :: Bool -> State -> State
strobe bool state@State{pressed} =
    if bool
    then state { mode = Strobing }
    else state { mode = Sampled $ map (`elem` pressed) [A .. Right] }

read :: State -> (Bool,State)
read state@State{mode,pressed} = case mode of
    Strobing -> (A `elem` pressed, state)
    Sampled bools -> case bools of
        [] -> (True, state)
        b1:bs' -> (b1, state { mode = Sampled bs' })
