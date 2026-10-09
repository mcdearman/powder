-- Haskell 2010 Report 9: (^) for non-negative integral exponents and (^^) for any integral exponent
module Main (main) where

import Data.Ratio ((%))

main :: IO ()
main = do
  print ("^ Int", (2 :: Int) ^ (10 :: Int), (3 :: Int) ^ (4 :: Int), (10 :: Int) ^ (18 :: Int))
  print ("^ zero exponent", (2 :: Int) ^ (0 :: Int), (0 :: Int) ^ (0 :: Int), (-5 :: Int) ^ (0 :: Int), (0.0 :: Double) ^ (0 :: Int))
  print ("^ one exponent", (7 :: Int) ^ (1 :: Int), (-7 :: Int) ^ (1 :: Int))
  print ("^ zero base", (0 :: Int) ^ (5 :: Int), (1 :: Int) ^ (1000000 :: Int))
  print ("^ negative base", (-2 :: Int) ^ (3 :: Int), (-2 :: Int) ^ (4 :: Int), (-1 :: Int) ^ (101 :: Int))
  print ("^ Int near overflow", (2 :: Int) ^ (62 :: Int), (2 :: Int) ^ (63 :: Int) == minBound)
  print ("^ Integer", (2 :: Integer) ^ (64 :: Int), (2 :: Integer) ^ (100 :: Integer), (-3 :: Integer) ^ (41 :: Int))
  print ("^ exponent types", (2 :: Int) ^ (5 :: Integer), (2 :: Integer) ^ (5 :: Int), (2 :: Double) ^ (5 :: Integer))
  print ("^ Double", (1.5 :: Double) ^ (2 :: Int), (0.5 :: Double) ^ (10 :: Int), (-2 :: Double) ^ (3 :: Int), (10 :: Double) ^ (3 :: Int))
  print ("^ Float", (1.5 :: Float) ^ (3 :: Int), (2 :: Float) ^ (24 :: Int))
  print ("^ Rational", (1 % 2 :: Rational) ^ (3 :: Int), (-2 % 3 :: Rational) ^ (2 :: Int), (5 % 7 :: Rational) ^ (0 :: Int))
  print ("^^ positive", (2 :: Double) ^^ (3 :: Int), (1.5 :: Double) ^^ (2 :: Int))
  print ("^^ zero", (2 :: Double) ^^ (0 :: Int), (0.5 :: Double) ^^ (0 :: Int))
  print ("^^ negative", (2 :: Double) ^^ (-2 :: Int), (0.5 :: Double) ^^ (-1 :: Int), (4 :: Double) ^^ (-1 :: Int), (-2 :: Double) ^^ (-3 :: Int))
  print ("^^ Float", (2 :: Float) ^^ (-3 :: Int), (0.25 :: Float) ^^ (-2 :: Int))
  print ("^^ Rational", (1 % 2 :: Rational) ^^ (-3 :: Int), (2 % 3 :: Rational) ^^ (-2 :: Int), (-2 % 3 :: Rational) ^^ (-3 :: Int), (3 :: Rational) ^^ (2 :: Int))
  print ("^^ exponent Integer", (2 :: Double) ^^ (-10 :: Integer), (10 :: Rational) ^^ (-2 :: Integer))
  print ("defaulting", 2 ^ 10, 2 ^^ (-1), 10 ^ 20)
  print ("sections", map (^ 2) [1, 2, 3 :: Int], map (2 ^) [0, 1, 2, 3 :: Int], map (2 ^^) [-2, -1, 0, 1 :: Int])
  print ("law", all (\n -> (2 :: Integer) ^ (n + 1) == 2 * 2 ^ n) [0 .. 20 :: Int])
