-- Haskell 2010 Report 10.6: unary minus is left-associative at level 6 and cannot combine with a non-associative level-6 operator
module Main (main) where

infix 6 <->

(<->) :: Int -> Int -> Int
a <-> b = a - b

main :: IO ()
main = print (- 1 <-> 2)
