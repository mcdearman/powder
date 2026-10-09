-- Haskell 2010 Report 10.5: parentheses must balance
module Main (main) where

main :: IO ()
main = print ((1 + 2) * (3 :: Int)))
