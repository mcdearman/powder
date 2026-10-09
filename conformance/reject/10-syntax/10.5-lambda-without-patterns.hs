-- Haskell 2010 Report 3.3 / 10.5: a lambda abstraction needs at least one pattern
module Main (main) where

main :: IO ()
main = print ((\ -> (1 :: Int)))
