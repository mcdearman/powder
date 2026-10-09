-- Haskell 2010 Report 2.5: a floating literal must have digits both before and after the decimal point (.5 is not a literal)
module Main (main) where

main :: IO ()
main = print (.5 + (1 :: Double))
