-- Haskell 2010 Report 11 (condition 3): REJECT. Deriving Show needs a Show instance for every field type, and there is
-- no Show instance for the function type Int -> Int.
module Main (main) where

data T = T (Int -> Int) deriving (Show)

main :: IO ()
main = return ()
