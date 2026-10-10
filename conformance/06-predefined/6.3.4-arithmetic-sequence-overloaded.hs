-- Haskell 2010 Report 6.3.4, 3.10: one arithmetic sequence of type (Enum a, Num a) => [a], used at several
-- types
module Main (main) where

import Prelude hiding (sum)

numbers :: (Enum a, Num a) => [a]
numbers = [1, 3 .. 10]

from :: (Enum a, Num a) => [a]
from = [1, 2 ..]

sum :: [Integer] -> Integer
sum = foldr (+) 0

main :: IO ()
main = do
  print (numbers :: [Int])
  print (numbers :: [Integer])
  print (numbers :: [Double])
  print (numbers :: [Rational])
  print (take 4 (from :: [Int]), take 4 (from :: [Integer]))
  print (sum numbers, sum (take 10 from))
