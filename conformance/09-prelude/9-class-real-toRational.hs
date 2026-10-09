-- Haskell 2010 Report 9: class Real (toRational) at Int, Integer, Float, Double and Rational
module Main (main) where

import Data.Ratio ((%), numerator, denominator)

main :: IO ()
main = do
  print ("Int", toRational (3 :: Int), toRational (-3 :: Int), toRational (0 :: Int))
  print ("Int bounds", toRational (maxBound :: Int), toRational (minBound :: Int))
  print ("Integer", toRational (2 ^ (70 :: Int) :: Integer), toRational (-7 :: Integer))
  print ("Double exact", toRational (0.75 :: Double), toRational (-2.5 :: Double), toRational (4 :: Double))
  print ("Double small", toRational (0.125 :: Double), toRational (0 :: Double))
  print ("Double inexact literal", toRational (0.1 :: Double))
  print ("Double 0.1 parts", numerator (toRational (0.1 :: Double)), denominator (toRational (0.1 :: Double)))
  print ("Double large", toRational (1.0e20 :: Double))
  print ("Float exact", toRational (0.75 :: Float), toRational (-2.5 :: Float))
  print ("Float inexact literal", toRational (0.1 :: Float))
  print ("Rational identity", toRational (3 % 4 :: Rational), toRational (-1 % 3 :: Rational))
  print ("round trip Double", fromRational (toRational (0.1 :: Double)) == (0.1 :: Double))
  print ("round trip Float", fromRational (toRational (0.1 :: Float)) == (0.1 :: Float))
  print ("round trip list", all (\x -> fromRational (toRational x) == x) [0, 1.5, -2.25, 1.0e-3, 123456.789 :: Double])
  print ("ordering preserved", toRational (0.1 :: Float) > toRational (0.1 :: Double))
  print ("mixed", toRational (2 :: Int) + toRational (0.5 :: Double) == 5 % 2)
