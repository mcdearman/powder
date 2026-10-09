-- Haskell 2010 Report 9 (and 6.4.1): REJECT. An integer literal has type Num a => a and Bool is not an instance of Num.
module Main (main) where

main :: IO ()
main = print (not 1)
