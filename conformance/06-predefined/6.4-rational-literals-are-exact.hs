-- Haskell 2010 Report 6.4.1, 2.5: a decimal literal at Rational is the exact fraction it is written as
module Main (main) where

import Data.Ratio (denominator, numerator)

half, sum' :: Rational
half = 0.5
sum' = 0.1 + 0.5

main :: IO ()
main = do
  print [numerator half == 1 && denominator half == 2, numerator sum' == 3 && denominator sum' == 5]
  print (0.5 :: Rational)
  print (0.1 + 0.2 == (0.3 :: Rational), 0.1 + 0.2 == (0.3 :: Double))
  print (1.5e2 :: Rational, 2.5e-1 :: Rational, 12.000 :: Rational)
  print (numerator (0.125 :: Rational), denominator (0.125 :: Rational))
  print (3.75 * 4 :: Rational, recip (0.25 :: Rational), negate 0.75 :: Rational)
