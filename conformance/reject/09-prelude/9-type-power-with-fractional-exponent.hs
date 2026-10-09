-- Haskell 2010 Report 9: REJECT. (^) :: (Num a, Integral b) => a -> b -> a needs an Integral exponent, and Double is
-- not an instance of Integral.
module Main (main) where

main :: IO ()
main = print ((2 :: Double) ^ (0.5 :: Double))
