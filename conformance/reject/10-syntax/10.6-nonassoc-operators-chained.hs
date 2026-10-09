-- Haskell 2010 Report 10.6: non-associative operators of the same precedence cannot be chained (a == b == c)
module Main (main) where

main :: IO ()
main = print (1 == (1 :: Int) == True)
