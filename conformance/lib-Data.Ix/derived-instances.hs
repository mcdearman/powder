-- Haskell 2010 Report 19.2 (Data.Ix): deriving Ix for enumerations and for single-constructor types whose
-- fields are in Ix (indexed like tuples)
module Main (main) where

import Data.Ix

data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
  deriving (Eq, Ord, Show, Ix)

data Unit = Unit
  deriving (Eq, Ord, Show, Ix)

data Point = Point Int Int
  deriving (Eq, Ord, Show, Ix)

data Cell = Cell { cellColour :: Colour, cellOn :: Bool }
  deriving (Eq, Ord, Show, Ix)

data Pair a b = Pair a b
  deriving (Eq, Ord, Show, Ix)

newtype Age = Age Int
  deriving (Eq, Ord, Show, Ix)

data Box = Box Point Colour
  deriving (Eq, Ord, Show, Ix)

main :: IO ()
main = do
  print ("enum range", range (Red, Violet))
  print ("enum range part", range (Yellow, Blue))
  print ("enum range singleton", range (Green, Green))
  print ("enum range empty", range (Blue, Yellow))
  print ("enum index", index (Red, Violet) Red, index (Red, Violet) Green, index (Red, Violet) Violet)
  print ("enum index offset", index (Yellow, Blue) Yellow, index (Yellow, Blue) Blue)
  print ("enum inRange", inRange (Yellow, Blue) Green, inRange (Yellow, Blue) Red, inRange (Yellow, Blue) Violet)
  print ("enum rangeSize", rangeSize (Red, Violet), rangeSize (Yellow, Blue), rangeSize (Blue, Yellow))
  print ("single nullary constructor", range (Unit, Unit), index (Unit, Unit) Unit, inRange (Unit, Unit) Unit, rangeSize (Unit, Unit))
  print ("product range", range (Point 0 0, Point 1 2))
  print ("product index", index (Point 1 1, Point 3 4) (Point 2 3), index (Point 1 1, Point 3 4) (Point 3 4))
  print ("product inRange", inRange (Point 0 0, Point 2 2) (Point 1 1), inRange (Point 0 0, Point 2 2) (Point 1 3))
  print ("product inRange is componentwise", inRange (Point 0 0, Point 2 2) (Point 1 9), Point 1 9 <= Point 2 2)
  print ("product rangeSize", rangeSize (Point 1 1, Point 3 4), rangeSize (Point 1 5, Point 3 2))
  print ("product agrees with tuple", map (\(Point x y) -> (x, y)) (range (Point 0 5, Point 2 6)) == range ((0, 5), (2, 6)))
  print ("record range", range (Cell Red False, Cell Orange True))
  print ("record index", index (Cell Red False, Cell Violet True) (Cell Orange True))
  print ("record fields", map cellOn (range (Cell Blue False, Cell Blue True)))
  print ("parameterised range", range (Pair 'a' False, Pair 'b' True))
  print ("parameterised index", index (Pair (0 :: Int) LT, Pair 9 GT) (Pair 4 EQ))
  print ("parameterised rangeSize", rangeSize (Pair Red (1 :: Integer), Pair Violet 10))
  print ("newtype range", range (Age 18, Age 21))
  print ("newtype index", index (Age 18, Age 21) (Age 20), inRange (Age 18, Age 21) (Age 17))
  print ("nested range", range (Box (Point 0 0) Red, Box (Point 0 1) Orange))
  print ("nested rangeSize", rangeSize (Box (Point 0 0) Red, Box (Point 9 9) Violet))
  print ("nested index", index (Box (Point 0 0) Red, Box (Point 9 9) Violet) (Box (Point 1 2) Yellow))
