-- Haskell 2010 Report 4.5.5 Rule 2: monomorphic constrained type variables left at the top level are resolved by defaulting
module Main (main) where

import Data.List (genericLength)

-- each of these is a restricted binding whose type is fixed by the default (Integer, Double)
n = 2 ^ 70

len = genericLength "abc"

lenNums = genericLength [1, 2, 3]

half = 1 / 2

xs = [1, 2, 3]

big = fromIntegral (maxBound :: Int) + 1

avg = sum [1, 2, 3, 4] / 4

-- uses len, so both stay at the same (defaulted) type
count = len + 1

(q, r) = 17 `divMod` 5

main :: IO ()
main = do
  print ("n is Integer", n)
  print ("len", len)
  print ("lenNums", lenNums)
  print ("half is Double", half)
  print ("xs", xs)
  print ("big does not wrap", big)
  print ("avg", avg)
  print ("count", count)
  print ("pattern binding", q, r)
  print ("n squared", n * n)
