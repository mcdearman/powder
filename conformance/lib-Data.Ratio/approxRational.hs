-- Haskell 2010 Report 22 (Data.Ratio): approxRational x eps is the simplest rational within eps of x
module Main (main) where

import Data.Ratio

q :: Rational -> Rational -> Rational
q = approxRational

within :: Rational -> Rational -> Rational -> Bool
within x eps r = abs (r - x) <= eps

main :: IO ()
main = do
  print ("pi to a hundredth", q (314159 % 100000) (1 % 100))
  print ("pi to a thousandth", q (314159 % 100000) (1 % 1000))
  print ("integer in range wins", q (31 % 10) (1 % 10))
  print ("zero epsilon returns argument", q (22 % 7) 0)
  print ("zero epsilon reduces", q (6 % 8) 0)
  print ("three tenths", q (3 % 10) (1 % 10))
  print ("negative", q ((-3) % 10) (1 % 10))
  print ("zero in range", q (1 % 4) (1 % 2), q ((-1) % 4) (1 % 2))
  print ("zero at the edge", q (1 % 2) (1 % 2), q ((-1) % 2) (1 % 2))
  print ("one third stays", q (1 % 3) (1 % 1000))
  print ("large epsilon", q (17 % 3) 10)
  print ("between integers", q (5 % 2) (2 % 5))
  print ("simplest is smallest denominator", q (27 % 100) (3 % 100))
  print ("results are within epsilon", all (\(x, e) -> within x e (q x e)) [(314159 % 100000, 1 % 1000), (2 % 7, 1 % 50), ((-9) % 13, 1 % 20), (99 % 100, 1 % 1000)])
  print ("Double pi 0.01", approxRational (pi :: Double) 0.01)
  print ("Double pi 0.001", approxRational (pi :: Double) 0.001)
  print ("Double half exact", approxRational (0.5 :: Double) 0)
  print ("Double zero", approxRational (0 :: Double) 0.5)
  print ("Double tenth", approxRational (0.1 :: Double) 0.01)
  print ("Double zero epsilon is toRational", approxRational (0.1 :: Double) 0 == toRational (0.1 :: Double))
  print ("Float exact", approxRational (0.75 :: Float) 0)
  print ("Float sqrt 2", approxRational (1.41421 :: Float) 0.01)
  print ("continued fraction convergents of 314159/100000", map (q (314159 % 100000)) [1, 1 % 10, 1 % 100, 1 % 1000, 1 % 100000])
