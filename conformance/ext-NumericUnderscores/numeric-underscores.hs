{-# LANGUAGE NumericUnderscores #-}
-- NumericUnderscores: underscores among the digits of a number, for the reader
module Main (main) where

x_1 :: Int
x_1 = 5

main :: IO ()
main = do
  print (1_000_000 :: Int, 12__34 :: Integer, 0x_ff_ff :: Int, 0o7_7 :: Int)
  print (1_000.000_5 :: Double, 1_0e1_0 :: Double, 6.022_140_76e+2_3 :: Double, 1_00e-0_2 :: Double)
  print (x_1, 1_0 + x_1, [1_0 .. 1_3 :: Int])
  print (0xDEAD_BEEF :: Integer, 1_2 `div` 5 :: Int)
  case 2_000 :: Int of
    2000 -> putStrLn "a pattern with underscores is the same number"
    _ -> putStrLn "not the same"
