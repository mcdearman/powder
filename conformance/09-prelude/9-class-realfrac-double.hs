-- Haskell 2010 Report 9: class RealFrac (properFraction truncate round ceiling floor) at Double; round is to nearest even
module Main (main) where

samples :: [Double]
samples = [-2.75, -2.5, -2.25, -2.0, -1.5, -0.5, -0.25, 0.0, 0.25, 0.5, 1.5, 2.0, 2.25, 2.5, 2.75, 3.5]

row :: Double -> IO ()
row x = print (x, truncate x :: Int, round x :: Int, ceiling x :: Int, floor x :: Int, properFraction x :: (Int, Double))

lawPF :: Double -> Bool
lawPF x = fromIntegral n + f == x && abs f < 1 && (f == 0 || signum f == signum x)
  where
    (n, f) = properFraction x :: (Integer, Double)

main :: IO ()
main = do
  mapM_ row samples
  print ("properFraction law", all lawPF samples)
  print ("to Integer", truncate (1.0e20 :: Double) :: Integer, round (-1.0e20 :: Double) :: Integer)
  print ("to Integer big", floor (1.5e30 :: Double) :: Integer, ceiling (2 ^^ (80 :: Int) :: Double) :: Integer)
  print ("round half even", map round [0.5, 1.5, 2.5, 3.5, 4.5 :: Double] :: [Integer])
  print ("round half even neg", map round [-0.5, -1.5, -2.5, -3.5, -4.5 :: Double] :: [Integer])
  print ("round near half", round (2.4999 :: Double) :: Int, round (2.5001 :: Double) :: Int)
  print ("ceiling/floor ints", ceiling (5 :: Double) :: Int, floor (5 :: Double) :: Int, ceiling (-5 :: Double) :: Int, floor (-5 :: Double) :: Int)
  print ("ceiling/floor tiny", ceiling (1.0e-10 :: Double) :: Int, floor (-1.0e-10 :: Double) :: Int, floor (1.0e-10 :: Double) :: Int)
  print ("truncate toward zero", truncate (-0.999 :: Double) :: Int, truncate (0.999 :: Double) :: Int, truncate (-7.9 :: Double) :: Int)
  print ("properFraction inexact", properFraction (3.7 :: Double) :: (Int, Double))
  print ("properFraction negative", properFraction (-3.75 :: Double) :: (Integer, Double))
  print ("defaulting", round 2.5, truncate (-2.5), ceiling 2.1, floor (-2.1))
