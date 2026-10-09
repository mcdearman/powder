module Paper (Paper(..), chainPaper) where

import Rock (Hand(..))
import Scissors (Scissors(..), chainScissors)

data Paper = Paper deriving (Show, Eq)

instance Hand Paper where
  name _ = "paper"
  beats _ = "rock"

chainPaper :: Int -> Paper -> [String]
chainPaper n p
  | n <= 0 = []
  | otherwise = name p : chainScissors (n - 1) Scissors
