-- Haskell 2010 Report 2.6: the empty escape \& is allowed in strings but not in character literals
module Main (main) where

main :: IO ()
main = print '\&'
