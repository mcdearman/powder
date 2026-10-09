-- Haskell 2010 Report 4.1.1: kind error: Int has kind * and cannot be applied to an argument
module Main (main) where

value :: Int Int
value = 0

main :: IO ()
main = putStrLn "unreachable"
