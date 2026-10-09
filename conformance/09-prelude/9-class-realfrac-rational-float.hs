-- Haskell 2010 Report 9: class RealFrac (properFraction truncate round ceiling floor) at Rational and Float
module Main (main) where

import Data.Ratio ((%))

samples :: [Rational]
samples = [-7 % 2, -5 % 2, -7 % 3, -2, -1 % 2, -1 % 3, 0, 1 % 3, 1 % 2, 2 % 3, 3 % 2, 2, 5 % 2, 8 % 3, 7 % 2]

row :: Rational -> IO ()
row x = print (x, truncate x :: Integer, round x :: Integer, ceiling x :: Integer, floor x :: Integer, properFraction x :: (Integer, Rational))

rowF :: Float -> IO ()
rowF x = print (x, truncate x :: Int, round x :: Int, ceiling x :: Int, floor x :: Int, properFraction x :: (Int, Float))

lawPF :: Rational -> Bool
lawPF x = fromIntegral n + f == x && abs f < 1 && (f == 0 || signum f == signum x)
  where
    (n, f) = properFraction x :: (Int, Rational)

main :: IO ()
main = do
  mapM_ row samples
  print ("properFraction law", all lawPF samples)
  print ("round half even", map round [1 % 2, 3 % 2, 5 % 2, 7 % 2 :: Rational] :: [Int])
  print ("round half even neg", map round [-1 % 2, -3 % 2, -5 % 2, -7 % 2 :: Rational] :: [Int])
  print ("large", truncate (10 ^ (30 :: Int) % 7 :: Rational) :: Integer, round (10 ^ (30 :: Int) % 7 :: Rational) :: Integer)
  print ("floor/ceiling large", floor (-(10 ^ (30 :: Int)) % 7 :: Rational) :: Integer, ceiling (10 ^ (30 :: Int) % 7 :: Rational) :: Integer)
  mapM_ rowF [-2.5, -1.5, -0.75, -0.5, 0, 0.5, 0.75, 1.5, 2.5, 100.25]
  print ("Float to Integer", round (1.0e10 :: Float) :: Integer, truncate (-16777216 :: Float) :: Integer)
