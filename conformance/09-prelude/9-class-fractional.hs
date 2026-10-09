-- Haskell 2010 Report 9: class Fractional ((/) recip fromRational) at Float, Double and Rational
module Main (main) where

import Data.Ratio ((%))

main :: IO ()
main = do
  print ("Double div", (1 :: Double) / 2, (7 :: Double) / 2, (-7 :: Double) / 2, (1 :: Double) / 3)
  print ("Double div exact", (0.75 :: Double) / 0.25, (10 :: Double) / 4, (0 :: Double) / 5)
  print ("Double div by zero", (1 :: Double) / 0, (-1 :: Double) / 0, isNaN ((0 :: Double) / 0))
  print ("Double recip", recip (4 :: Double), recip (0.5 :: Double), recip (-8 :: Double), recip (3 :: Double))
  print ("Double fromRational", fromRational (3 % 4) :: Double, fromRational (-7 % 2) :: Double, fromRational (1 % 3) :: Double)
  print ("Double literal is fromRational", (0.1 :: Double) == fromRational (1 % 10), (2.5e-3 :: Double) == fromRational (25 % 10000))
  print ("Float div", (1 :: Float) / 2, (1 :: Float) / 3, (-7 :: Float) / 2)
  print ("Float recip", recip (4 :: Float), recip (3 :: Float))
  print ("Float fromRational", fromRational (3 % 4) :: Float, fromRational (1 % 10) :: Float)
  print ("Float div by zero", (1 :: Float) / 0, isNaN ((0 :: Float) / 0))
  print ("Rational div", (1 :: Rational) / 2, (3 % 4) / (5 % 6 :: Rational), (-1 :: Rational) / 3)
  print ("Rational recip", recip (3 % 4 :: Rational), recip (-3 % 4 :: Rational), recip (5 :: Rational))
  print ("Rational fromRational", fromRational (3 % 4) :: Rational)
  print ("Rational literal", 0.75 :: Rational, 1.5e-2 :: Rational, 2.0 :: Rational)
  print ("recip law", all (\x -> recip x == 1 / x) [0.5, 2, -4, 3 :: Double], all (\x -> recip x * x == 1) [1 % 2, 3, -5 % 7 :: Rational])
  print ("defaulting", 1 / 4, recip 8, 3 / 2 + 1)
