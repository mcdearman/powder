-- Haskell 2010 Report 3.13 (REJECT): all alternatives of a case expression must have the same type
module Main (main) where

main :: IO ()
main = print (case True of { True -> 't'; False -> "f" })
