-- Haskell 2010 Report 9 (and 4.4.2, 10.6): REJECT. The Prelude declares `infix 4 ==`: the operator is non-associative,
-- so `a == b == c` is a syntax error without parentheses.
module Main (main) where

main :: IO ()
main = print (True == True == True)
