module Rock (Rock(..), Hand(..), chainFrom) where

import qualified Paper

data Rock = Rock deriving (Show, Eq)

class Hand a where
  name :: a -> String
  beats :: a -> String

instance Hand Rock where
  name _ = "rock"
  beats _ = "scissors"

-- the hand that beats the hand that beats ... (n steps), starting from Rock
chainFrom :: Int -> Rock -> [String]
chainFrom n r
  | n <= 0 = []
  | otherwise = name r : Paper.chainPaper (n - 1) Paper.Paper
