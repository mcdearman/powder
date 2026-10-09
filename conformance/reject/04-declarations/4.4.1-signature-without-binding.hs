-- Haskell 2010 Report 4.4.1: a type signature must be accompanied by a binding for the variable in the same declaration list
module Main (main) where

orphan :: Int -> Int

main :: IO ()
main = putStrLn "unreachable"
