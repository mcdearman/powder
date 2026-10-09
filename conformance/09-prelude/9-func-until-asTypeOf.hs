-- Haskell 2010 Report 9: until and asTypeOf
module Main (main) where

collatz :: Int -> Int
collatz n = if even n then n `div` 2 else 3 * n + 1

steps :: Int -> Int
steps n = snd (until ((== 1) . fst) (\(m, k) -> (collatz m, k + 1)) (n, 0))

main :: IO ()
main = do
  print ("until doubles", until (> 100) (* 2) (1 :: Int))
  print ("until already true", until (> 100) (* 2) (500 :: Int))
  print ("until predicate true: function not used", until (const True) (undefined :: Int -> Int) 5)
  print ("until counts down", until (<= 0) (subtract 3) (10 :: Int))
  print ("until on strings", until ((>= 5) . length) ('x' :) "")
  print ("until on pairs", until (\(a, _) -> a > 50) (\(a, b) -> (b, a + b)) (0 :: Integer, 1 :: Integer))
  print ("until collatz", map steps [1, 2, 3, 6, 7, 27])
  print ("until on Double", until (< 1) (/ 2) (64 :: Double))
  print ("until sqrt", until (\x -> x * x <= 50) (subtract 1) (50 :: Int))
  print ("asTypeOf value", asTypeOf (3 :: Int) 4, 'a' `asTypeOf` 'b')
  print ("asTypeOf second not forced", (5 :: Int) `asTypeOf` undefined)
  print ("asTypeOf fixes literal", 3 `asTypeOf` (1.5 :: Double), 3 `asTypeOf` (0 :: Int))
  print ("asTypeOf fixes read", read "42" `asTypeOf` (0 :: Int), read "[True]" `asTypeOf` [False])
  print ("asTypeOf fixes toEnum", toEnum 65 `asTypeOf` 'x', toEnum 1 `asTypeOf` False)
  print ("asTypeOf fixes minBound", minBound `asTypeOf` 'x' == '\0', maxBound `asTypeOf` LT)
  print ("asTypeOf fixes empty list", [] `asTypeOf` "abc", length ([] `asTypeOf` [()]))
  print ("asTypeOf = const", asTypeOf 'p' 'q' == const 'p' 'q')
