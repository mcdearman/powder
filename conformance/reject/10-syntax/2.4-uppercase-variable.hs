-- Haskell 2010 Report 2.4: a name beginning with an upper-case letter is a constructor, so it cannot be bound as a function
module Main (main) where

Double x = x * 2

main :: IO ()
main = print (Double (2 :: Int))
