
module Honesty.Six502.Cycles (Cycles) where

newtype Cycles = Cycles { n :: Int } deriving (Eq,Ord,Num)

instance Show Cycles where show Cycles{n} = "CYC:" <> show n
