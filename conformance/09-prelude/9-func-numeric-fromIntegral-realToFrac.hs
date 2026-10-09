-- Haskell 2010 Report 9: fromIntegral (= fromInteger . toInteger) and realToFrac (= fromRational . toRational)
module Main (main) where

import Data.Ratio ((%))

average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

main :: IO ()
main = do
  print ("Int -> Integer", fromIntegral (5 :: Int) :: Integer, fromIntegral (minBound :: Int) :: Integer)
  print ("Integer -> Int", fromIntegral (5 :: Integer) :: Int, fromIntegral (-5 :: Integer) :: Int)
  print ("Integer -> Int wraps", fromIntegral (2 ^ (64 :: Int) + 3 :: Integer) :: Int, fromIntegral (2 ^ (63 :: Int) :: Integer) :: Int)
  print ("Int -> Int", fromIntegral (7 :: Int) :: Int)
  print ("Int -> Double", fromIntegral (3 :: Int) :: Double, fromIntegral (-3 :: Int) :: Double, fromIntegral (maxBound :: Int) :: Double)
  print ("Int -> Float", fromIntegral (3 :: Int) :: Float, fromIntegral (16777216 :: Int) :: Float)
  print ("Integer -> Double", fromIntegral (10 ^ (20 :: Int) :: Integer) :: Double, fromIntegral (2 ^ (53 :: Int) :: Integer) :: Double)
  print ("Int -> Rational", fromIntegral (3 :: Int) :: Rational, fromIntegral (-3 :: Integer) :: Rational)
  print ("average", average [1, 2, 3, 4], average [1, 2])
  print ("length to Double", fromIntegral (length "hello") * (0.5 :: Double))
  print ("law", (fromIntegral (42 :: Int) :: Integer) == fromInteger (toInteger (42 :: Int)))
  print ("Double -> Double", realToFrac (1.5 :: Double) :: Double)
  print ("Float -> Double exact", realToFrac (1.5 :: Float) :: Double, realToFrac (-0.25 :: Float) :: Double)
  print ("Float -> Double inexact", realToFrac (0.1 :: Float) :: Double)
  print ("Double -> Float", realToFrac (1.5 :: Double) :: Float, realToFrac (0.1 :: Double) :: Float)
  print ("Double -> Float rounds", realToFrac (1.00000001 :: Double) :: Float)
  print ("Int -> Double", realToFrac (3 :: Int) :: Double, realToFrac (-3 :: Integer) :: Float)
  print ("Rational -> Double", realToFrac (3 % 4 :: Rational) :: Double, realToFrac (1 % 3 :: Rational) :: Double)
  print ("Double -> Rational", realToFrac (0.75 :: Double) :: Rational, realToFrac (-2.5 :: Float) :: Rational)
  print ("law", (realToFrac (0.3 :: Float) :: Double) == fromRational (toRational (0.3 :: Float)))
  print ("round trip", (realToFrac (realToFrac (0.1 :: Float) :: Double) :: Float) == 0.1)
