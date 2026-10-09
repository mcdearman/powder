-- Haskell 2010 Report 2.4: reserved operators (here `=>`) cannot be defined as ordinary operator symbols
module Main (main) where

(=>) :: Int -> Int -> Int
a => b = a + b

main :: IO ()
main = print (1 => 2)
