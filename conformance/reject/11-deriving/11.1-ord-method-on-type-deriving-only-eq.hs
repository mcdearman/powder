-- Haskell 2010 Report 11.1: REJECT. Only the classes named in the deriving clause get instances: T derives Eq but not
-- Ord, so (<) cannot be used at T.
module Main (main) where

data T = A | B deriving (Eq, Show)

main :: IO ()
main = print (A < B)
