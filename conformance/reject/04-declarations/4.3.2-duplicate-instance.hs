-- Haskell 2010 Report 4.3.2: a type may not be declared an instance of a particular class more than once
module Main (main) where

data T = T

instance Show T where
  show T = "first"

instance Show T where
  show T = "second"

main :: IO ()
main = putStrLn "unreachable"
