-- Haskell 2010 Report 20, 5.3: sort is exported by Data.List, not by the Prelude; using it without
-- importing Data.List is a scope error
module Main (main) where

main :: IO ()
main = print (sort [3, 1, 2 :: Int])
