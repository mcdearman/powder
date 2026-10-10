{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
-- BinaryLiterals: numbers written in binary; with NumericUnderscores, underscores among their digits
module Main (main) where

main :: IO ()
main = do
  print (0b0 :: Int, 0b1 :: Int, 0b1010 :: Int, 0B1111 :: Integer)
  print (0b1111_0000 :: Int, 0b_1 :: Int, 0b1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0 :: Integer)
  print (0b101 + 0b10 :: Int, map (* 0b10) [1, 2, 3 :: Int], 0b11 == (3 :: Int))
  print (fromIntegral (0b1001 :: Int) :: Double)
  case 0b100 :: Int of
    4 -> putStrLn "a pattern in binary is the same number"
    _ -> putStrLn "not the same"
