-- Haskell 2010 Report 3.8 (REJECT): tuples of different sizes have different types
module Main (main) where

first :: (Int, Int) -> Int
first (a, _, _) = a

main :: IO ()
main = print (first (1, 2))
