-- Haskell 2010 Report 4.4.3.1: all clauses defining a function must have the same number of argument patterns
module Main (main) where

f :: Int -> Int -> Int
f 0 = \y -> y
f x y = x + y

main :: IO ()
main = print (f 1 2)
