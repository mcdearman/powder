-- Haskell 2010 Report 19.1 (Data.Ix): empty ranges (lower bound above upper bound), including tuples where
-- only one component is empty
module Main (main) where

import Data.Ix

main :: IO ()
main = do
  print ("range Int", range (5, 1 :: Int))
  print ("range Int adjacent", range (1, 0 :: Int))
  print ("rangeSize Int", rangeSize (5, 1 :: Int), rangeSize (1, 0 :: Int), rangeSize (maxBound, minBound :: Int))
  print ("inRange Int", inRange (5, 1 :: Int) 3, inRange (5, 1 :: Int) 5, inRange (5, 1 :: Int) 1)
  print ("range Integer", range (10 ^ 30, 0 :: Integer))
  print ("rangeSize Integer", rangeSize (10 ^ 30, 0 :: Integer))
  print ("range Char", range ('z', 'a'))
  print ("rangeSize Char", rangeSize ('z', 'a'))
  print ("inRange Char", inRange ('z', 'a') 'm')
  print ("range Bool", range (True, False))
  print ("rangeSize Bool", rangeSize (True, False))
  print ("inRange Bool", inRange (True, False) True, inRange (True, False) False)
  print ("range Ordering", range (GT, LT))
  print ("rangeSize Ordering", rangeSize (GT, LT), rangeSize (EQ, LT))
  print ("pair empty in first component", range ((2, 0), (1, 5) :: (Int, Int)))
  print ("pair empty in second component", range ((1, 5), (3, 2) :: (Int, Int)))
  print ("pair empty in both components", range ((2, 5), (1, 2) :: (Int, Int)))
  print ("rangeSize pair empty in first", rangeSize ((2, 0), (1, 5) :: (Int, Int)))
  print ("rangeSize pair empty in second", rangeSize ((1, 5), (3, 2) :: (Int, Int)))
  print ("rangeSize pair upper bound lexicographically larger", rangeSize ((0, 1), (5, 0) :: (Int, Int)))
  print ("inRange pair", inRange ((1, 5), (3, 2) :: (Int, Int)) (2, 3), inRange ((1, 5), (3, 2) :: (Int, Int)) (1, 5))
  print ("triple empty in middle", range ((0, 1, 0), (5, 0, 5) :: (Int, Int, Int)))
  print ("rangeSize triple empty in middle", rangeSize ((0, 1, 0), (5, 0, 5) :: (Int, Int, Int)))
  print ("rangeSize triple empty in last", rangeSize ((0, 0, 1), (5, 5, 0) :: (Int, Int, Int)))
  print ("mixed pair empty", range (('b', False), ('a', True)), rangeSize (('a', True), ('b', False)))
  print ("rangeSize equals length of range", all (\b -> rangeSize b == length (range b)) [(5, 1), (1, 0), (0, 0), (0, 3), (-3, -5 :: Int)])
  print ("nothing is in an empty range", not (any (inRange (4, 2 :: Int)) [0 .. 6]))
