-- Haskell 2010 Report 4.4.2: an operator may have at most one fixity declaration
module Main (main) where

infixl 6 <+>
infixr 5 <+>

(<+>) :: Int -> Int -> Int
a <+> b = a + b

main :: IO ()
main = print (1 <+> 2)
