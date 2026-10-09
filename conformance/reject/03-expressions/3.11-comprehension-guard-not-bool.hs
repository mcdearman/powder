-- Haskell 2010 Report 3.11 (REJECT): a guard in a list comprehension must have type Bool
module Main (main) where

main :: IO ()
main = print [x | x <- "abc", 'c']
