-- Haskell 2010 Report 3.13 (REJECT): every pattern of a case must have the type of the scrutinee
module Main (main) where

main :: IO ()
main = print (case 'a' of { True -> "bool"; _ -> "other" })
