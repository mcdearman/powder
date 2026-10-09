-- Haskell 2010 Report 4.3.4: only one default declaration is permitted per module
module Main (main) where

default (Int)

default (Double)

main :: IO ()
main = putStrLn "unreachable"
