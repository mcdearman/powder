module Even (isEven, evens) where

import Odd (isOdd, odds)

isEven :: Int -> Bool
isEven 0 = True
isEven n = isOdd (n - 1)

-- a lazily, mutually recursive pair of top-level values across modules
evens :: [Int]
evens = 0 : map (+ 1) odds
