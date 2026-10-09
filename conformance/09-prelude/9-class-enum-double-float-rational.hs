-- Haskell 2010 Report 9 (and 6.3.4): class Enum at Double, Float and Rational; numericEnumFromTo runs to limit + 1/2
-- (only steps that are exact in binary are used, so every element is exact)
module Main (main) where

import Data.Ratio ((%))

main :: IO ()
main = do
  print ("succ/pred", succ (1.5 :: Double), pred (1.5 :: Double), succ (-0.5 :: Float), pred (0.25 :: Float))
  print ("fromEnum truncates", fromEnum (3.7 :: Double), fromEnum (-3.7 :: Double), fromEnum (0.5 :: Float), fromEnum (2.0 :: Double))
  print ("toEnum", toEnum 3 :: Double, toEnum (-2) :: Float, toEnum 0 :: Double)
  print ("enumFromTo", [1.0 .. 5.0 :: Double], enumFromTo (1.0 :: Double) 3.0)
  print ("enumFromTo overshoot", [1.0 .. 3.5 :: Double], [1.0 .. 3.25 :: Double], [0.5 .. 3.0 :: Double])
  print ("enumFromTo just below half", [1.0 .. 3.4375 :: Double])
  print ("enumFromTo empty/single", [3.0 .. 1.0 :: Double], [2.0 .. 2.0 :: Double], [2.0 .. 2.25 :: Double], [2.0 .. 1.5 :: Double])
  print ("enumFromThenTo up", [1.0, 1.5 .. 3.0 :: Double], enumFromThenTo (0.0 :: Double) 0.25 1.0)
  print ("enumFromThenTo overshoot", [1.0, 3.0 .. 8.0 :: Double], [1.0, 3.0 .. 7.5 :: Double])
  print ("enumFromThenTo down", [5.0, 4.0 .. 1.0 :: Double], [3.0, 2.5 .. 1.0 :: Double], [5.0, 3.0 .. 0.0 :: Double])
  print ("enumFromThenTo empty", [1.0, 2.0 .. 0.0 :: Double], [1.0, 0.0 .. 2.0 :: Double])
  print ("enumFrom", take 3 (enumFrom (0.5 :: Double)), take 3 [-1.0 :: Double ..])
  print ("enumFromThen", take 4 (enumFromThen (1.0 :: Double) 0.75), take 3 [0.0, 0.125 :: Double ..])
  print ("Float", [1.0 .. 3.5 :: Float], [1.0, 1.25 .. 2.0 :: Float], take 3 [0.5 :: Float ..])
  print ("Float down", [2.0, 1.5 .. 0.0 :: Float], fromEnum (-0.75 :: Float))
  print ("Rational succ/pred", succ (1 % 2 :: Rational), pred (1 % 2 :: Rational))
  print ("Rational from/to", fromEnum (7 % 2 :: Rational), fromEnum (-7 % 2 :: Rational), toEnum 3 :: Rational)
  print ("Rational enumFromTo", [1 % 2 .. 5 % 2 :: Rational], [1 % 2 .. 3 :: Rational])
  print ("Rational enumFromThenTo", [0, 1 % 3 .. 1 :: Rational], [1, 2 % 3 .. 0 :: Rational])
  print ("Rational enumFrom", take 3 [1 % 3 :: Rational ..], take 3 [1 % 3, 1 % 2 :: Rational ..])
  print ("length", length [1.0 .. 100.0 :: Double], sum [0.5, 1.0 .. 10.0 :: Double])
