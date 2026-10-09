-- Haskell 2010 Report 3.16 (REJECT): the declared type in e :: t may not be more general than the principal type of e
module Main (main) where

main :: IO ()
main = print ((not :: a -> a) True)
