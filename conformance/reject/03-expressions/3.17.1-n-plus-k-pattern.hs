-- Haskell 2010 Report 3.17.1 (REJECT): n+k patterns were removed in Haskell 2010
module Main (main) where

predecessor :: Int -> Int
predecessor (n + 1) = n
predecessor _ = 0

main :: IO ()
main = print (predecessor 5)
