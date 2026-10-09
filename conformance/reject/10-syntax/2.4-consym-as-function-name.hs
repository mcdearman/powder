-- Haskell 2010 Report 2.4: an operator symbol starting with a colon is a constructor and cannot be defined as a function
module Main (main) where

(:+) :: Int -> Int -> Int
a :+ b = a + b

main :: IO ()
main = print (1 :+ 2)
