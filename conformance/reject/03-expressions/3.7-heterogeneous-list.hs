-- Haskell 2010 Report 3.7 (REJECT): all elements of a list must have the same type
module Main (main) where

main :: IO ()
main = print [True, 'a']
