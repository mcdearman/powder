-- Haskell 2010 Report 4.3.4: default (Double) makes ambiguous numeric types Double
module Main (main) where

default (Double)

main :: IO ()
main = do
  print ("literal", 7)
  print ("negative literal", -7)
  print ("list", [1, 2, 3])
  print ("sum", sum [1, 2, 3])
  print ("division", 7 / 2)
  print ("pow with Int exponent", 2 ^ (10 :: Int))
  print ("big pow", 2 ^ (64 :: Int))
  print ("enum", [1 .. 3])
  print ("fromIntegral", fromIntegral (3 :: Int))
  print ("negate", negate 2)
  print ("truncate", truncate 2.5 :: Int)
  print ("eq", 1 == 1)
  print ("recip", recip 4)
  print ("show", show 12 ++ "/" ++ show (3 * 4))
  print ("max", max 1 2)
