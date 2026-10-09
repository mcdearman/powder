-- Haskell 2010 Report 4.2.1: the type variables of a data declaration head must be distinct
module Main (main) where

data T a a = MkT a

main :: IO ()
main = putStrLn "unreachable"
