-- Haskell 2010 Report 4.2.1: a field label shared by several constructors must have the same type in all of them
module Main (main) where

data T = A { field :: Int } | B { field :: Bool }

main :: IO ()
main = putStrLn "unreachable"
