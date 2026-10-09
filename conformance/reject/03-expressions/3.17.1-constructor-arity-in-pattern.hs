-- Haskell 2010 Report 3.17.1 (REJECT): the arity of a constructor must match the number of sub-patterns
module Main (main) where

first :: Maybe Int -> Int
first (Just x y) = x
first Nothing = 0

main :: IO ()
main = print (first (Just 1))
