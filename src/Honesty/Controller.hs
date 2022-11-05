
module Honesty.Controller( -- keyboard-mapping is in Sim
    Button(..),
    State, state0,
    Effect(..),
    inter,
    showPressed,
    ) where

import Prelude hiding (Left,Right)

import Data.Set (Set)
import Honesty.Byte (Byte)
import qualified Honesty.Log as Log (Effect)

data State = Strobing | Sampled [(Button,Bool)] deriving (Show)

state0 :: State
state0 = Sampled []

data Effect a where
    Read :: Effect Byte
    Strobe :: Bool -> Effect ()

data Button = A | B | Select | Start | Up | Down | Left | Right deriving (Eq,Ord,Enum,Show)

type Pressed = Set Button -- only J1

inter :: Pressed -> State -> Effect a -> Log.Effect (a,State)
inter pressed state = \case
    Strobe True -> return ((), Strobing)
    Strobe False -> return ((), Sampled $ map (\b -> (b, b `elem` pressed)) [A .. Right])
    Read ->
        case state of
            Strobing -> return (bool2byte $ A `elem` pressed, Strobing)
            Sampled bools -> case bools of
                [] -> return (1, Sampled [])
                (_but,down):bs' -> do
                    --when down $ Log.message $ show _but
                    return (bool2byte down, Sampled bs')

bool2byte :: Bool -> Byte
bool2byte b = if b then 1 else 0

showPressed :: Set Button -> String
showPressed pressed =
    map (\b -> if b `elem` pressed then letter b else '-') [A .. Right]
    where letter = \case
              A -> 'A' ; B -> 'B' ; Select -> 'T' ; Start -> 'E'
              Up -> 'U' ; Down -> 'D' ; Left -> 'L' ; Right -> 'R'
