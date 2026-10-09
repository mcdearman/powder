-- Haskell 2010 Report 4.2.3: a newtype declaration must have exactly one constructor
module Main (main) where

newtype N = A Int | B Int

main :: IO ()
main = putStrLn "unreachable"
