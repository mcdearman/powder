-- Haskell 2010 Report 3.6 / 10.5: a conditional expression must have an else branch
module Main (main) where

main :: IO ()
main = print (if True then (1 :: Int))
