-- Haskell 2010 Report 4.3.2 / 4.1.4: (+) requires a Num instance, and there is no instance Num Bool
module Main (main) where

main :: IO ()
main = print (True + False)
