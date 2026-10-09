-- Haskell 2010 Report 3.6 (REJECT): both branches of a conditional must have the same type
module Main (main) where

main :: IO ()
main = print (if True then 'a' else "b")
