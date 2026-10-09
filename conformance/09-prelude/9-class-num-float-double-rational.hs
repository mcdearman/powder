-- Haskell 2010 Report 9: class Num at Float, Double and Rational ((+) (-) (*) negate abs signum fromInteger)
module Main (main) where

import Data.Ratio ((%))

half :: Rational
half = 1 % 2

third :: Rational
third = 1 % 3

main :: IO ()
main = do
  print ("Double add", (1.5 :: Double) + 2.25, (0.1 :: Double) + 0.2, (1.0e16 :: Double) + 1)
  print ("Double sub", (1.5 :: Double) - 2.25, (0.3 :: Double) - 0.1)
  print ("Double mul", (1.5 :: Double) * 2.5, (-0.5 :: Double) * 4, (1.0e200 :: Double) * 1.0e200)
  print ("Double negate", negate (1.5 :: Double), negate (-1.5 :: Double), -(2.5 :: Double))
  print ("Double abs", abs (-1.5 :: Double), abs (1.5 :: Double), abs (0 :: Double))
  print ("Double signum", signum (-2.5 :: Double), signum (0 :: Double), signum (2.5 :: Double))
  print ("Double fromInteger", fromInteger 3 :: Double, fromInteger (-3) :: Double, fromInteger (2 ^ (60 :: Int)) :: Double)
  print ("Double literal", (3 :: Double), (3 :: Double) == 3.0)
  print ("Float add", (1.5 :: Float) + 2.25, (0.1 :: Float) + 0.2, (16777216 :: Float) + 1)
  print ("Float sub/mul", (1.5 :: Float) - 2.25, (1.5 :: Float) * 2.5, (0.1 :: Float) * 3)
  print ("Float negate/abs", negate (1.5 :: Float), abs (-1.5 :: Float), abs (0.25 :: Float))
  print ("Float signum", signum (-2.5 :: Float), signum (0 :: Float), signum (2.5 :: Float))
  print ("Float fromInteger", fromInteger 3 :: Float, fromInteger 16777217 :: Float)
  print ("Rational add", half + third, half + half, third + third + third)
  print ("Rational sub", half - third, third - half, half - half)
  print ("Rational mul", half * third, (2 % 3) * (3 % 2 :: Rational), (-half) * half)
  print ("Rational negate", negate half, negate (negate third), -(3 % 4 :: Rational))
  print ("Rational abs", abs (negate half), abs half, abs (0 :: Rational))
  print ("Rational signum", signum (negate half), signum (0 :: Rational), signum third)
  print ("Rational fromInteger", fromInteger 5 :: Rational, fromInteger (-5) :: Rational, (7 :: Rational))
  print ("Rational normalised", 2 % 4 :: Rational, 6 % (-8) :: Rational, 0 % 5 :: Rational)
  print ("abs * signum", all (\x -> abs x * signum x == x) [-2.5, -0.5, 0.5, 2.5 :: Double], all (\x -> abs x * signum x == x) [negate half, 0, third])
