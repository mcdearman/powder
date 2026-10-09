-- Haskell 2010 Report 16, 5.3: ord is exported by Data.Char, not by the Prelude; using it without
-- importing Data.Char is a scope error
module Main (main) where

main :: IO ()
main = print (ord 'a')
