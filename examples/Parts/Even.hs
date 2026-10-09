-- Half of a pair of modules that import each other.
module Parts.Even (isEven, Parity (..), describe) where

import Parts.Odd (isOdd)

data Parity = Even | Odd deriving (Eq, Show)

isEven :: Int -> Bool
isEven 0 = True
isEven n = isOdd (n - 1)

describe :: Int -> Parity
describe n = if isEven n then Even else Odd

-- Not exported.
secret :: Int
secret = 42
