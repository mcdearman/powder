-- Haskell 2010 Report 18, 5.3: the type Int8 is exported by Data.Int, not by the Prelude; using it
-- without importing Data.Int is a scope error
module Main (main) where

small :: Int8
small = 5

main :: IO ()
main = print small
