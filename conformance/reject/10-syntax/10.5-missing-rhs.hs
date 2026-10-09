-- Haskell 2010 Report 4.4.3 / 10.5: a binding needs a right-hand side
module Main (main) where

f :: Int -> Int
f x =

main :: IO ()
main = print (f 1)
