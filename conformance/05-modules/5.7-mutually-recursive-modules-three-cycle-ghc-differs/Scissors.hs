module Scissors (Scissors(..), chainScissors, rockAgain, module Rock) where

import Rock

data Scissors = Scissors deriving (Show, Eq)

instance Hand Scissors where
  name _ = "scissors"
  beats _ = "paper"

chainScissors :: Int -> Scissors -> [String]
chainScissors n s
  | n <= 0 = []
  | otherwise = name s : chainFrom (n - 1) Rock

rockAgain :: Rock
rockAgain = Rock
