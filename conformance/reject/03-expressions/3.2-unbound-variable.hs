-- Haskell 2010 Report 3.2 (REJECT): a variable must be in scope where it is used
module Main (main) where

main :: IO ()
main = print (notDefinedAnywhere + (1 :: Int))
