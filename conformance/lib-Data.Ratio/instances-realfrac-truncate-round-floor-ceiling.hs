-- Haskell 2010 Report 22 (Data.Ratio): RealFrac instance of Ratio: properFraction, truncate, round
-- (to even on ties), ceiling, floor, in particular on negative ratios
module Main (main) where

import Data.Ratio

samples :: [Rational]
samples = [7 % 2, (-7) % 2, 5 % 2, (-5) % 2, 1 % 2, (-1) % 2, 3 % 2, (-3) % 2, 7 % 3, (-7) % 3, 8 % 3, (-8) % 3, 4, -4, 0, 1 % 100, (-1) % 100]

pf :: Rational -> (Integer, Rational)
pf = properFraction

main :: IO ()
main = do
  print ("samples", samples)
  print ("truncate", map truncate samples :: [Integer])
  print ("round", map round samples :: [Integer])
  print ("ceiling", map ceiling samples :: [Integer])
  print ("floor", map floor samples :: [Integer])
  print ("properFraction positive", pf (7 % 2))
  print ("properFraction negative", pf ((-7) % 2))
  print ("properFraction thirds", pf (8 % 3), pf ((-8) % 3))
  print ("properFraction integer", pf 4, pf (-4))
  print ("properFraction small", pf (1 % 100), pf ((-1) % 100))
  print ("properFraction zero", pf 0)
  print ("properFraction recombines", all (\r -> (\(n, f) -> fromInteger n + f == r) (pf r)) samples)
  print ("fraction has the sign of the argument", all (\r -> signum (snd (pf r)) `elem` [0, signum r]) samples)
  print ("fraction is smaller than one", all (\r -> abs (snd (pf r)) < 1) samples)
  print ("truncate is integer part", all (\r -> truncate r == fst (pf r)) samples)
  print ("floor <= truncate <= ceiling", all (\r -> (floor r :: Integer) <= truncate r && (truncate r :: Integer) <= ceiling r) samples)
  print ("round ties go to even", map round [1 % 2, 3 % 2, 5 % 2, 7 % 2, 9 % 2 :: Rational] :: [Integer])
  print ("round negative ties go to even", map round [(-1) % 2, (-3) % 2, (-5) % 2, (-7) % 2 :: Rational] :: [Integer])
  print ("round non ties", map round [49 % 100, 51 % 100, (-49) % 100, (-51) % 100, 249 % 100, 251 % 100 :: Rational] :: [Integer])
  print ("result at Int", (truncate ((-7) % 2 :: Rational) :: Int), (round (5 % 2 :: Rational) :: Int), (floor ((-7) % 2 :: Rational) :: Int), (ceiling ((-7) % 2 :: Rational) :: Int))
  print ("Ratio Int", (truncate ((-7) % 2 :: Ratio Int) :: Int), (round ((-7) % 2 :: Ratio Int) :: Int), (floor ((-7) % 2 :: Ratio Int) :: Int), (ceiling ((-7) % 2 :: Ratio Int) :: Int))
  print ("large", (floor ((10 ^ (30 :: Int) + 1) % 2 :: Rational) :: Integer), (ceiling ((10 ^ (30 :: Int) + 1) % 2 :: Rational) :: Integer))
  print ("floor and ceiling differ by one unless integral", map (\r -> (ceiling r :: Integer) - floor r) samples)
