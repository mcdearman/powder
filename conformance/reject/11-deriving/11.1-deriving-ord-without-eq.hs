-- Haskell 2010 Report 11 (and 4.3.2): REJECT. Ord has superclass Eq, so an Ord instance (derived or not) requires an
-- Eq instance for the type; none is declared or derived here.
module Main (main) where

data T = A | B deriving (Ord)

main :: IO ()
main = return ()
