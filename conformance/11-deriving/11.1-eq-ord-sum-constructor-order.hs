-- Haskell 2010 Report 11.1: derived Ord on a type with several constructors: an earlier constructor is smaller than a later
-- one whatever the fields are; values of the same constructor are compared field by field
module Main (main) where

import Data.List (sort)

data Shape = Circle Int | Rect Int Int | Dot | Tri Int Int Int deriving (Eq, Ord, Show)

data Token = TInt Int | TStr String | TEnd | TOp Char deriving (Eq, Ord, Show)

shapes :: [Shape]
shapes = [Circle 5, Circle 100, Rect 0 0, Rect 0 1, Rect 1 0, Dot, Tri 0 0 0, Tri 0 0 1]

main :: IO ()
main = do
  print ("different constructors are never equal", Circle 1 == Rect 1 1, Dot == Tri 0 0 0, Circle 1 /= Dot)
  print ("same constructor, same fields", Circle 1 == Circle 1, Rect 1 2 == Rect 1 2, Dot == Dot, Tri 1 2 3 == Tri 1 2 3)
  print ("same constructor, different fields", Circle 1 == Circle 2, Rect 1 2 == Rect 2 1, Tri 1 2 3 == Tri 1 2 4)
  print ("earlier constructor is smaller", Circle 100 < Rect 0 0, Rect 99 99 < Dot, Dot < Tri (-1) (-1) (-1), Circle maxBound < Tri minBound 0 0)
  print ("later constructor is bigger", Tri 0 0 0 > Dot, Dot > Rect 5 5, Rect 0 0 > Circle 7, Tri 0 0 0 >= Circle 0)
  print ("compare across constructors", compare (Circle 9) Dot, compare Dot (Circle 9), compare Dot Dot, compare (Tri 0 0 0) (Rect 9 9))
  print ("within a constructor", compare (Circle 5) (Circle 100), compare (Rect 0 1) (Rect 1 0), compare (Tri 1 2 3) (Tri 1 2 2), Rect 1 0 <= Rect 1 0)
  print ("compare table", [[compare a b | b <- shapes] | a <- shapes])
  print ("the sample list is sorted", sort (reverse shapes) == shapes, and (zipWith (<) shapes (tail shapes)))
  print ("sort", sort [Dot, Tri 1 1 1, Circle 2, Rect 3 4, Circle 1, Dot, Rect 3 3])
  print ("max/min", max (Circle 100) Dot, min (Circle 100) Dot, maximum shapes, minimum shapes)
  print ("tokens", sort [TOp '+', TEnd, TStr "b", TInt 2, TStr "a", TInt 1, TOp '*'])
  print ("tokens compare", compare (TInt 99) (TStr ""), compare TEnd (TStr "zzz"), compare (TOp 'a') TEnd, TStr "ab" < TStr "b")
  print ("consistency", and [(a < b) == (compare a b == LT) && (a == b) == (compare a b == EQ) && (a > b) == (compare a b == GT) | a <- shapes, b <- shapes])
  print ("transitivity", and [not (a <= b && b <= c) || a <= c | a <- shapes, b <- shapes, c <- shapes])
