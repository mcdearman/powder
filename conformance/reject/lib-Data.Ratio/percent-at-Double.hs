-- Haskell 2010 Report 22: (%) :: Integral a => a -> a -> Ratio a; Double is not an instance of Integral
module Main (main) where

import Data.Ratio

x :: Double
x = 1.5

main :: IO ()
main = print (numerator (x % 2))
