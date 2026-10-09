-- Haskell 2010 Report 4.3.2: an instance declaration may contain bindings only, not type signatures
module Main (main) where

data T = T

instance Show T where
  show :: T -> String
  show T = "T"

main :: IO ()
main = print T
