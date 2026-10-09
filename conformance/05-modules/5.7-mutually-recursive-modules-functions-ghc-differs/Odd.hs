module Odd (isOdd, odds) where

import Even (isEven, evens)

isOdd :: Int -> Bool
isOdd 0 = False
isOdd n = isEven (n - 1)

odds :: [Int]
odds = map (+ 1) evens
