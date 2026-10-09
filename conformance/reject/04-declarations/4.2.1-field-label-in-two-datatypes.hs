-- Haskell 2010 Report 4.2.1: a field label cannot be shared by more than one type in scope
module Main (main) where

data P = P { label :: Int }

data Q = Q { label :: Int }

main :: IO ()
main = putStrLn "unreachable"
