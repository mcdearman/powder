-- Haskell 2010 Report 14.1.1, 14.2 (Data.Array): accumArray and accum
module Main (main) where

import Data.Array

histogram :: (Int, Int) -> [Int] -> Array Int Int
histogram bnds xs = accumArray (+) 0 bnds [(x, 1) | x <- xs, inRange bnds x]

buckets :: Array Int String
buckets = accumArray (\acc c -> acc ++ [c]) "" (0, 2) [(0, 'a'), (2, 'b'), (0, 'c'), (2, 'd'), (0, 'e')]

base :: Array Int Int
base = listArray (0, 3) [100, 200, 300, 400]

main :: IO ()
main = do
  print ("histogram", elems (histogram (0, 4) [1, 3, 3, 0, 4, 3, 9, -1, 1]))
  print ("histogram assocs", assocs (histogram (1, 3) [1, 3, 3]))
  print ("initial value kept", elems (accumArray (+) 7 (0, 2) [] :: Array Int Int))
  print ("accumulation is left to right", elems buckets)
  print ("non commutative", elems (accumArray (-) 100 (0, 1) [(0, 1), (0, 2), (1, 50)] :: Array Int Int))
  print ("flip const keeps last", elems (accumArray (flip const) 'z' (0, 2) [(1, 'a'), (1, 'b')] :: Array Int Char))
  print ("const keeps initial", elems (accumArray const 'z' (0, 2) [(1, 'a'), (1, 'b')] :: Array Int Char))
  print ("different element types", elems (accumArray (\n s -> n + length s) 0 (1, 2) [(1, "ab"), (2, "c"), (1, "def")] :: Array Int Int))
  print ("accum", elems (accum (+) base [(0, 1), (3, 4), (0, 10)]))
  print ("accum empty list", accum (+) base [] == base)
  print ("accum non commutative", elems (accum (-) base [(1, 50), (1, 25)]))
  print ("accum different types", elems (accum (\n c -> n + fromEnum c) base [(2, 'A')]))
  print ("accum keeps bounds", bounds (accum (+) base [(0, 1)]))
  print ("original unchanged", elems base)
