-- Haskell 2010 Report 4.2.3: a newtype constructor must have exactly one field
module Main (main) where

newtype N = N Int Int

main :: IO ()
main = putStrLn "unreachable"
