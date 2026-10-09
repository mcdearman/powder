-- Haskell 2010 Report 4.4.3.1: the clauses of a function binding must be contiguous; another
-- declaration between them makes the second clause a duplicate definition
module Main (main) where

f :: Int -> Int
f 0 = 0

g :: Int
g = 5

f n = n + 1

main :: IO ()
main = print (f g)
