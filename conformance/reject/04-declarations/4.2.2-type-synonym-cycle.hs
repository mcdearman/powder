-- Haskell 2010 Report 4.2.2: type synonyms may not be recursive unless an algebraic datatype intervenes
module Main (main) where

type A = [B]

type B = Maybe A

main :: IO ()
main = putStrLn "unreachable"
