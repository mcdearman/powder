-- Haskell 2010 Report 4.3.3: a deriving clause names a class that is not in scope
module Main (main) where

data T = T deriving (Printable)

main :: IO ()
main = putStrLn "unreachable"
