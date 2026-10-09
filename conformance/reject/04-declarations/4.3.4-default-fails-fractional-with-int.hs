-- Haskell 2010 Report 4.3.4: with `default (Int)` no default type satisfies (Show a, Fractional a), so the
-- type of `1 / 2` stays ambiguous
module Main (main) where

default (Int)

main :: IO ()
main = print (1 / 2)
