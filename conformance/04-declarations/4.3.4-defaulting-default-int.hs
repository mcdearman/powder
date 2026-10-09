-- Haskell 2010 Report 4.3.4: default (Int) makes ambiguous numeric types Int (64-bit wraparound is visible)
module Main (main) where

default (Int)

main :: IO ()
main = do
  print ("literal", 7)
  print ("pow", 2 ^ 10)
  print ("pow 63 wraps", 2 ^ 63)
  print ("pow 64 wraps", 2 ^ 64)
  print ("mul wraps", 4611686018427387904 * 4)
  print ("add wraps", 9223372036854775807 + 1)
  print ("big literal wraps", 18446744073709551617)
  print ("list", [1, 2, 3])
  print ("div", 7 `div` 2)
  print ("sum", sum [1 .. 10])
  print ("fromIntegral", fromIntegral (maxBound :: Int) + 1)
  print ("fromIntegral from Integer", fromIntegral (2 ^ 40 * (2 :: Integer) ^ (24 :: Int)))
  print ("show", show 12 ++ show (3 * 4))
