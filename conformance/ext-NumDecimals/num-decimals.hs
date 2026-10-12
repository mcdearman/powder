{-# LANGUAGE NumDecimals #-}
-- NumDecimals: a number written with an exponent, if it is whole, is a whole number of any type that has them
module Main (main) where

million :: Int
million = 1e6

big :: Integer
big = 1.5e20

classify :: Int -> String
classify 1e3 = "a thousand"
classify 2.5e1 = "twenty-five"
classify _ = "something else"

main :: IO ()
main = do
  print (million, big)
  print (1e3 + 1 :: Int, 12.5e1 :: Integer)
  print (replicate 0.3e1 'x')
  print (map classify [1000, 25, 7])
  -- A number that is not whole is a fraction as ever, and so is any at a type that has fractions.
  print (1.5 :: Double, 1e3 :: Double, 2.5e-1 :: Double)
  print (1e2 `div` (3 :: Int))
