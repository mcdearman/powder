-- Haskell 2010 Report 4.3.4: default (Double, Integer): the first type in the list satisfying all constraints is chosen
module Main (main) where

default (Double, Integer)

main :: IO ()
main = do
  print ("Num only picks Double", 7)
  print ("list picks Double", [1, 2])
  print ("Fractional picks Double", 1 / 2)
  print ("Integral skips Double", 7 `div` 2)
  print ("Integral gcd", gcd 12 18)
  print ("Integral even", even 4)
  print ("toInteger", toInteger 5)
  print ("base Double exponent Integer", 2 ^ 10)
  print ("both in one expression", fromIntegral (7 `div` 2))
  print ("big Integer", 2 ^ 64 `div` 3)
  print ("RealFrac then Integral", round 2.5, truncate (-2.5))
  print ("Enum picks Double", [1 .. 3])
  print ("Enum Integral", [1 .. 3 `div` 1])
