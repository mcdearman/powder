-- Haskell 2010 Report 4.3.4: every type in a default declaration must be an instance of Num; Bool is not
module Main (main) where

default (Bool)

main :: IO ()
main = putStrLn "unreachable"
