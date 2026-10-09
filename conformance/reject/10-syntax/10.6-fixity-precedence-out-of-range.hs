-- Haskell 2010 Report 4.4.2: the precedence in a fixity declaration is a single digit 0-9
module Main (main) where

infixl 10 <+>

(<+>) :: Int -> Int -> Int
a <+> b = a + b

main :: IO ()
main = print (1 <+> 2)
