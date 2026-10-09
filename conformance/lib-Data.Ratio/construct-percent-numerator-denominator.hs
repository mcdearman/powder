-- Haskell 2010 Report 22 (Data.Ratio): Ratio, Rational, (%), numerator, denominator: reduction to lowest
-- terms, sign carried by the numerator, Ratio Int, fixity infixl 7
module Main (main) where

import Data.Int
import Data.Ratio

parts :: Rational -> (Integer, Integer)
parts r = (numerator r, denominator r)

main :: IO ()
main = do
  print ("simple", 3 % 4 :: Rational)
  print ("reduced", 6 % 8 :: Rational)
  print ("reduced to integer", 10 % 5 :: Rational)
  print ("zero", 0 % 7 :: Rational)
  print ("negative numerator", (-3) % 4 :: Rational)
  print ("negative denominator", 3 % (-4) :: Rational)
  print ("both negative", (-3) % (-4) :: Rational)
  print ("negative reduced", 6 % (-8) :: Rational)
  print ("zero with negative denominator", 0 % (-5) :: Rational)
  print ("numerator denominator", parts (3 % 4))
  print ("parts are reduced", parts (6 % 8), parts (100 % 10), parts (0 % 9))
  print ("sign is in the numerator", parts (3 % (-4)), parts ((-3) % (-4)), parts ((-6) % 8))
  print ("denominator is positive", all (\r -> denominator r > 0) [3 % (-4), (-3) % 4, 0 % (-1), 5 % 1 :: Rational])
  print ("parts are coprime", all (\r -> gcd (numerator r) (denominator r) == 1) [6 % 8, 100 % 75, (-9) % 3, 0 % 5 :: Rational])
  print ("rebuild", (\r -> numerator r % denominator r == r) (22 % 7 :: Rational))
  print ("large", (2 ^ (70 :: Int)) % (2 ^ (68 :: Int)) :: Rational)
  print ("large coprime", parts ((10 ^ (20 :: Int) + 1) % (10 ^ (20 :: Int))))
  print ("Ratio Int", 6 % 8 :: Ratio Int)
  print ("Ratio Int negative", 3 % (-6) :: Ratio Int)
  print ("Ratio Int parts", numerator (9 % 12 :: Ratio Int), denominator (9 % 12 :: Ratio Int))
  print ("Ratio Int8", 100 % 120 :: Ratio Int8)
  print ("Ratio Integer is Rational", (1 % 2 :: Ratio Integer) == (1 % 2 :: Rational))
  print ("from variables", [n % d | n <- [1, 2], d <- [2, 4 :: Integer]])
  -- infixl 7 %
  print ("percent binds tighter than plus", 1 % 2 + 1 % 3 :: Rational)
  print ("percent is left associative with times", 2 * 3 % 4 :: Rational)
  print ("percent then times", 3 % 4 * 2 :: Rational)
  print ("power binds tighter than percent", 2 ^ (3 :: Int) % 4 :: Rational)
  print ("percent vs comparison", 1 % 2 < (2 % 3 :: Rational))
  print ("percent with negation", -1 % 2 :: Rational)
  print ("integer literal as Rational", (5 :: Rational), numerator (5 :: Rational), denominator (5 :: Rational))
  print ("fractional literal as Rational", (0.75 :: Rational), (1.0e-2 :: Rational), (3.14 :: Rational))
