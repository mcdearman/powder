-- Haskell 2010 Report 4.4.3: a variable may be bound only once in a declaration group
module Main (main) where

x :: Int
x = 1

y :: Int
y = 2

x = 3

main :: IO ()
main = print (x + y)
