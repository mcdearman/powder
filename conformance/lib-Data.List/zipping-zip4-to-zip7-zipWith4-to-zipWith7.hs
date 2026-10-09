-- Haskell 2010 Report 20.8 (Data.List): zip4, zip5, zip6, zip7 and zipWith4, zipWith5, zipWith6, zipWith7
module Main (main) where

import Data.List

as1 :: [Int]
as1 = [1, 2, 3]

bs1 :: String
bs1 = "abc"

cs1 :: [Bool]
cs1 = [True, False, True]

ds1 :: [Ordering]
ds1 = [LT, EQ, GT]

es1 :: [Double]
es1 = [0.5, 1.5, 2.5]

fs1 :: [String]
fs1 = ["x", "yy", "zzz"]

gs1 :: [Integer]
gs1 = [100, 200, 300]

main :: IO ()
main = do
  print ("zip4", zip4 as1 bs1 cs1 ds1)
  print ("zip4 shortest wins", zip4 as1 bs1 [True] ds1)
  print ("zip4 empty", zip4 as1 "" cs1 ds1)
  print ("zip4 infinite", zip4 [1 :: Int ..] (cycle "ab") (repeat True) [LT, GT])
  print ("zip5", zip5 as1 bs1 cs1 ds1 es1)
  print ("zip5 shortest wins", zip5 as1 bs1 cs1 ds1 [9.5])
  print ("zip6", zip6 as1 bs1 cs1 ds1 es1 fs1)
  print ("zip6 shortest wins", zip6 [7] bs1 cs1 ds1 es1 fs1)
  print ("zip7", zip7 as1 bs1 cs1 ds1 es1 fs1 gs1)
  print ("zip7 shortest wins", zip7 as1 bs1 cs1 ds1 es1 fs1 (take 2 gs1))
  print ("zip7 empty", zip7 as1 bs1 cs1 ds1 es1 fs1 ([] :: [Integer]))
  print ("zipWith4", zipWith4 (\a b c d -> a + b + c + d) as1 as1 as1 as1)
  print ("zipWith4 mixed", zipWith4 (\a b c d -> (show a ++ [b], c, d)) as1 bs1 cs1 ds1)
  print ("zipWith4 shortest wins", zipWith4 (\a b c d -> a + b + c + d) as1 [10] as1 as1)
  print ("zipWith4 is zip4", zipWith4 (,,,) as1 bs1 cs1 ds1 == zip4 as1 bs1 cs1 ds1)
  print ("zipWith5", zipWith5 (\a b c d e -> a + b + c + d + e) as1 as1 as1 as1 as1)
  print ("zipWith5 mixed", zipWith5 (\a b c d e -> (fromIntegral a + e, b, c, d)) as1 bs1 cs1 ds1 es1)
  print ("zipWith5 is zip5", zipWith5 (,,,,) as1 bs1 cs1 ds1 es1 == zip5 as1 bs1 cs1 ds1 es1)
  print ("zipWith6", zipWith6 (\a b c d e f -> a + b + c + d + e + f) as1 as1 as1 as1 as1 as1)
  print ("zipWith6 mixed", zipWith6 (\a b c d e f -> replicate a b ++ f ++ show (c, d, e)) as1 bs1 cs1 ds1 es1 fs1)
  print ("zipWith6 is zip6", zipWith6 (,,,,,) as1 bs1 cs1 ds1 es1 fs1 == zip6 as1 bs1 cs1 ds1 es1 fs1)
  print ("zipWith7", zipWith7 (\a b c d e f g -> a + b + c + d + e + f + g) as1 as1 as1 as1 as1 as1 as1)
  print ("zipWith7 mixed", zipWith7 (\a b c d e f g -> (toInteger a + g, b, c, d, e, f)) as1 bs1 cs1 ds1 es1 fs1 gs1)
  print ("zipWith7 shortest wins", zipWith7 (\a b c d e f g -> a + b + c + d + e + f + g) as1 as1 as1 [1] as1 as1 as1)
  print ("zipWith7 is zip7", zipWith7 (,,,,,,) as1 bs1 cs1 ds1 es1 fs1 gs1 == zip7 as1 bs1 cs1 ds1 es1 fs1 gs1)
  print ("zipWith7 infinite", take 2 (zipWith7 (\a b c d e f g -> a + b + c + d + e + f + g) [1 :: Int ..] [1 ..] [1 ..] [1 ..] [1 ..] [1 ..] [1 ..]))
