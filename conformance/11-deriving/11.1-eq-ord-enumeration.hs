-- Haskell 2010 Report 11.1: derived Eq and Ord for an enumeration; constructors are ordered as written in the declaration
module Main (main) where

import Data.List (sort)

-- deliberately not in alphabetical order
data Size = Tiny | Small | Medium | Large | Huge deriving (Eq, Ord, Show)

-- Eq only
data Coin = Heads | Tails deriving (Eq, Show)

sizes :: [Size]
sizes = [Tiny, Small, Medium, Large, Huge]

main :: IO ()
main = do
  print ("== table", [[a == b | b <- sizes] | a <- sizes])
  print ("/= row", [Medium /= b | b <- sizes])
  print ("compare table", [[compare a b | b <- sizes] | a <- sizes])
  print ("< row", [Medium < b | b <- sizes])
  print ("<= row", [Medium <= b | b <- sizes])
  print ("> row", [Medium > b | b <- sizes])
  print (">= row", [Medium >= b | b <- sizes])
  print ("declaration order, not alphabetical", Tiny < Small, Large < Huge, Huge > Tiny, compare Small Medium)
  print ("max/min", max Small Large, min Small Large, max Huge Huge, min Tiny Huge)
  print ("maximum/minimum", maximum [Medium, Huge, Tiny], minimum [Medium, Huge, Tiny])
  print ("sort", sort [Huge, Tiny, Large, Small, Medium, Tiny])
  print ("sort reversed", sort (reverse sizes) == sizes)
  print ("in lists", [Tiny, Huge] < [Small], [Tiny, Huge] < [Tiny, Huge, Tiny], [Large] == [Large])
  print ("in tuples and Maybe", (Small, 'b') < (Small, 'c'), Just Tiny < Just Small, Nothing < Just Tiny)
  print ("elem/lookup", Large `elem` sizes, lookup Medium (zip sizes [1 :: Int ..]), filter (> Small) sizes)
  print ("Eq only", Heads == Heads, Heads == Tails, Heads /= Tails, Tails `elem` [Heads], [Heads, Tails] == [Heads, Tails])
  print ("reflexive/symmetric", all (\a -> a == a) sizes, and [(a == b) == (b == a) | a <- sizes, b <- sizes])
  print ("antisymmetric", and [(compare a b == LT) == (compare b a == GT) | a <- sizes, b <- sizes])
  print ("consistent", and [(a < b) == (compare a b == LT) && (a <= b) == (compare a b /= GT) | a <- sizes, b <- sizes])
