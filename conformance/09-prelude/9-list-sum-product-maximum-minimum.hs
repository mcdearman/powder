-- Haskell 2010 Report 9 (PreludeList): sum, product, maximum, minimum
-- (maximum = foldl1 max and minimum = foldl1 min, so on ties maximum returns the last and minimum the first)
module Main (main) where

import Data.Ratio ((%))

-- ordered by the Int only; the Char tells equal elements apart
data Tagged = Tagged Int Char

instance Eq Tagged where
  Tagged a _ == Tagged b _ = a == b

instance Ord Tagged where
  Tagged a _ <= Tagged b _ = a <= b

tag :: Tagged -> Char
tag (Tagged _ c) = c

ties :: [Tagged]
ties = [Tagged 1 'a', Tagged 2 'b', Tagged 1 'c', Tagged 2 'd']

main :: IO ()
main = do
  print ("sum", sum [1, 2, 3 :: Int], sum [1 .. 100 :: Integer], sum [0.5, 0.25 :: Double], sum [1 % 2, 1 % 3 :: Rational])
  print ("sum empty", sum ([] :: [Int]), sum ([] :: [Integer]), sum ([] :: [Double]), sum ([] :: [Float]), sum ([] :: [Rational]))
  print ("sum singleton/negative", sum [7 :: Int], sum [-1, -2, 3 :: Int], sum [-0.5 :: Double])
  print ("sum Integer large", sum (replicate 10 (10 ^ (30 :: Int))) :: Integer)
  print ("sum Int wraps", sum [maxBound, 1 :: Int] == minBound)
  print ("product", product [1, 2, 3, 4 :: Int], product [1 .. 20 :: Integer], product [0.5, 4 :: Double], product [1 % 2, 2 % 3 :: Rational])
  print ("product empty", product ([] :: [Int]), product ([] :: [Integer]), product ([] :: [Double]), product ([] :: [Float]), product ([] :: [Rational]))
  print ("product singleton/zero/negative", product [7 :: Int], product [3, 0, 5 :: Int], product [-1, -2, -3 :: Int])
  print ("product Integer large", product [1 .. 25 :: Integer])
  print ("maximum", maximum [3, 1, 4, 1, 5, 9, 2, 6 :: Int], maximum [-3, -1, -2 :: Int], maximum [2.5, -1 :: Double])
  print ("minimum", minimum [3, 1, 4, 1, 5, 9, 2, 6 :: Int], minimum [-3, -1, -2 :: Int], minimum [2.5, -1 :: Double])
  print ("singleton", maximum [7 :: Int], minimum [7 :: Int], maximum "x", minimum [()])
  print ("all equal", maximum [4, 4, 4 :: Int], minimum [4, 4, 4 :: Int])
  print ("chars and strings", maximum "hello", minimum "hello", maximum ["apple", "pear", "fig"], minimum ["apple", "pear", "fig"])
  print ("string prefixes", maximum ["ab", "abc", "a"], minimum ["ab", "abc", "a"], maximum ["", "a"], minimum ["", "a"])
  print ("tuples", maximum [(1 :: Int, 'b'), (2, 'a'), (1, 'z')], minimum [(1 :: Int, 'b'), (2, 'a'), (1, 'a')])
  print ("lists", maximum [[1, 2], [1, 2, 3], [0, 9 :: Int]], minimum [[1, 2], [1, 2, 3], [0, 9 :: Int]])
  print ("Maybe and Bool", maximum [Just 1, Nothing, Just (3 :: Int)], minimum [Just 1, Nothing, Just (3 :: Int)], maximum [False, True], minimum [LT, GT, EQ])
  print ("Integer and Rational", maximum [2 ^ (70 :: Int), 2 ^ (69 :: Int) :: Integer], minimum [1 % 2, 1 % 3, 2 % 5 :: Rational])
  print ("maximum on ties returns the last", tag (maximum ties), tag (maximum [Tagged 5 'x', Tagged 5 'y', Tagged 5 'z']))
  print ("minimum on ties returns the first", tag (minimum ties), tag (minimum [Tagged 5 'x', Tagged 5 'y', Tagged 5 'z']))
  print ("large", maximum [1 .. 100000 :: Int], minimum [1 .. 100000 :: Int], sum [1 .. 100000 :: Int])
  print ("average", sum [1, 2, 3, 4 :: Double] / fromIntegral (length [1, 2, 3, 4 :: Int]))
