-- Haskell 2010 Report 3.13 / 10.5: a case expression must have at least one alternative
module Main (main) where

f :: Bool -> Int
f b = case b of {}

main :: IO ()
main = print (f True)
