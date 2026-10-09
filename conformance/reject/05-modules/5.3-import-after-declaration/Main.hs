-- Haskell 2010 Report 5.1 / 10.5: all import declarations must precede the top-level declarations
module Main (main) where

x :: Int
x = 1

import Lib

main :: IO ()
main = print (x + visible)
