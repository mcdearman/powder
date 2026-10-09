-- Haskell 2010 Report 3.6 (REJECT): the condition of a conditional must have type Bool
module Main (main) where

main :: IO ()
main = print (if 'c' then "yes" else "no")
