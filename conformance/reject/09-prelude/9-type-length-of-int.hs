-- Haskell 2010 Report 9: REJECT. length :: [a] -> Int cannot be applied to an Int.
module Main (main) where

main :: IO ()
main = print (length (5 :: Int))
