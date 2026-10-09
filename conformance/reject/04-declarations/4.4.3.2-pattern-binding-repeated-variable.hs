-- Haskell 2010 Report 4.4.3.2: a pattern binding may not bind the same variable twice
module Main (main) where

main :: IO ()
main = print (let (a, a) = (1 :: Int, 2 :: Int) in a)
