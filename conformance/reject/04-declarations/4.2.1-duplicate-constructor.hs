-- Haskell 2010 Report 4.2.1: a constructor name may be declared only once in a module, even across datatypes
module Main (main) where

data A = X | Y

data B = X | Z

main :: IO ()
main = putStrLn "unreachable"
