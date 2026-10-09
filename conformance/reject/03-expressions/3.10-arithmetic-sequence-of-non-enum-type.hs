-- Haskell 2010 Report 3.10 (REJECT): the elements of an arithmetic sequence must belong to an instance of class Enum
module Main (main) where

main :: IO ()
main = print (length [not .. not])
