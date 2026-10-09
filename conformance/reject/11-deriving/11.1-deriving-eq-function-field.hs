-- Haskell 2010 Report 11 (condition 3): REJECT. Deriving Eq needs an Eq instance for every field type, and there is
-- no Eq instance for the function type Int -> Int.
module Main (main) where

data T = T Int (Int -> Int) deriving (Eq)

main :: IO ()
main = return ()
