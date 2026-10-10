-- Haskell 2010 Report 3.4: prefix negation is negate, applies to literals and to parenthesised negations,
-- and binds as an operator of precedence 6
module Main (main) where

negativeFive :: Int
negativeFive = -5

doubleNegate :: Int -> Int
doubleNegate x = -(-x)

main :: IO ()
main = do
  print (negativeFive, doubleNegate 7, doubleNegate negativeFive)
  print (- 3 + 4 :: Int, -3 * 4 :: Int, -2 ^ (2 :: Int) :: Int, (-2) ^ (2 :: Int) :: Int)
  print (-(-(-1)) :: Integer, - (2 - 5) :: Integer, 4 - (-1) :: Integer, subtract 1 (-1) :: Int)
  print (-1.5 :: Double, -(1 / 4) :: Rational, negate (-0.0) :: Double, abs (-7) :: Int)
  print (let n = 6 :: Int in (-n, - n `div` 4, (-n) `div` 4, -n `mod` 4, (-n) `mod` 4))
