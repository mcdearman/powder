-- Haskell 2010 Report 2.5: floating literals: decimal point with digits on both sides, exponents e/E with sign
module Main (main) where

import Data.Ratio (numerator, denominator)

main :: IO ()
main = do
  print ("simple", 0.0, 1.5, 3.25, 10.0)
  print ("leading zeros", 00.5, 01.50)
  print ("exponent no point", 1e3, 1E3, 2e0, 5e1)
  print ("exponent with sign", 1e+3, 1e-3, 1E+2, 1E-2)
  print ("point and exponent", 1.5e2, 1.5E2, 2.5e-1, 0.25e+2)
  print ("big exponent", 1.0e10, 1.0e22, 1.0e-10)
  print ("small", 0.1, 0.01, 0.001, 0.0001)
  print ("show thresholds", 0.1, 9999999.0, 10000000.0, 0.09)
  print ("at Float", 1.5 :: Float, 0.1 :: Float, 1e10 :: Float)
  print ("negated", -1.5, -1e3, -2.5e-1)
  let r = 0.75 :: Rational
  print ("rational literal 0.75", numerator r, denominator r)
  let s = 1.25e2 :: Rational
  print ("rational literal 1.25e2", numerator s, denominator s)
  let t = 1e-2 :: Rational
  print ("rational literal 1e-2", numerator t, denominator t)
  let u = 0.1 :: Rational
  print ("rational literal 0.1 is exact", numerator u, denominator u)
  print ("float literal is Fractional not Integral", 2.0 / 4)
  -- "1.e" is not a float literal: 1 . e would be composition; "e1" alone is an identifier
  let e1 = 7 :: Int
  print ("e1 is an identifier", e1)
  print ("range with floats", [1.0, 1.5 .. 3.0])
  print ("int dot dot needs no space issue", [1..3], [1 ..3], [1.. 3])
