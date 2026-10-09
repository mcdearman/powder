-- Haskell 2010 Report 3.4 / 4.4.2 (REJECT): == is non-associative (infix 4), so a == b == c does not parse
module Main (main) where

main :: IO ()
main = print (True == True == True)
