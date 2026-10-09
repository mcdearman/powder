-- Haskell 2010 Report 4.3.3: deriving Eq requires Eq for every constructor field type; functions have no Eq instance
module Main (main) where

data F = F (Int -> Int) deriving Eq

main :: IO ()
main = putStrLn "unreachable"
