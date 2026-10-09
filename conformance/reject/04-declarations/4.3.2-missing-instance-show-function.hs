-- Haskell 2010 Report 4.3.2 / 4.1.4: print requires a Show instance, and there is none for function types
module Main (main) where

main :: IO ()
main = print not
