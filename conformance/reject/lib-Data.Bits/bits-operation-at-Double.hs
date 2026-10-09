-- Haskell 2010 Report 15: (.&.) is a method of class Bits; Double is not an instance of Bits
module Main (main) where

import Data.Bits

x :: Double
x = 6.0

main :: IO ()
main = print (x .&. 3.0)
