-- Haskell 2010 Report 4.4.1: it is invalid to give more than one type signature for one variable, even if identical
module Main (main) where

f :: Int -> Int
f :: Int -> Int
f x = x

main :: IO ()
main = print (f 1)
