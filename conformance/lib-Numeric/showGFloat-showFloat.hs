-- Haskell 2010 Report 38.1 (Numeric): showGFloat (fixed notation for 0.1 <= x < 10^7, exponent notation
-- otherwise) and showFloat (shortest digits that uniquely identify the number)
module Main (main) where

import Numeric

g :: Maybe Int -> Double -> String
g d x = showGFloat d x ""

s :: Double -> String
s x = showFloat x ""

main :: IO ()
main = do
  putStrLn (g Nothing 0.5)
  putStrLn (g Nothing 1234.5)
  putStrLn (g Nothing 0.1)
  putStrLn (g Nothing 9999999)
  putStrLn (g Nothing 1.0e7)
  putStrLn (g Nothing 12345678)
  putStrLn (g Nothing 0.05)
  putStrLn (g Nothing 0.099)
  putStrLn (g Nothing 1.0e-3)
  putStrLn (g Nothing 0)
  putStrLn (g Nothing (-0.5))
  putStrLn (g Nothing (-1.0e8))
  putStrLn (g (Just 2) 1234.5678)
  putStrLn (g (Just 2) 0.123)
  putStrLn (g (Just 2) 1.0e8)
  putStrLn (g (Just 2) 0.0123)
  putStrLn (g (Just 3) 9999999)
  putStrLn (g (Just 3) 123456789)
  putStrLn (g (Just 1) (-42.42))
  putStrLn (g (Just 1) (-0.0042))
  putStrLn (showGFloat (Just 2) (2.71828 :: Float) "")
  putStrLn (showGFloat Nothing (1.0e-2 :: Float) " units")
  putStrLn (s 1.5)
  putStrLn (s 0.1)
  putStrLn (s 1)
  putStrLn (s 0)
  putStrLn (s 100)
  putStrLn (s 1234567.0)
  putStrLn (s 9999999)
  putStrLn (s 1.0e7)
  putStrLn (s 0.05)
  putStrLn (s 1.0e-2)
  putStrLn (s 123456.789)
  putStrLn (s (0.1 + 0.2))
  putStrLn (s (1 / 3))
  putStrLn (s 5.0e-324)
  putStrLn (s 1.7976931348623157e308)
  putStrLn (s (-2.5))
  putStrLn (s (-1.0e-3))
  putStrLn (s (1 / 0))
  putStrLn (s (-1 / 0))
  putStrLn (s (0 / 0))
  putStrLn (showFloat (1.5 :: Float) "")
  putStrLn (showFloat (0.1 :: Float) "")
  putStrLn (showFloat (3.4028235e38 :: Float) "")
  putStrLn (showFloat (1 / 3 :: Float) "")
  putStrLn (showFloat (2.5 :: Double) " rest")
  print ("showFloat agrees with show on non-negative values", all (\x -> s x == show x) [0, 0.1, 1, 1.5, 100, 1.0e7, 1.0e-2, 123456.789, 0.1 + 0.2])
  print ("showFloat does not parenthesise negatives", s (-2.5), showsPrec 7 (-2.5 :: Double) "")
