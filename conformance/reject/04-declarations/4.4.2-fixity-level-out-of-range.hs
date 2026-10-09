-- Haskell 2010 Report 4.4.2: the precedence level in a fixity declaration must be in the range 0 to 9
module Main (main) where

infixl 10 <+>

(<+>) :: Int -> Int -> Int
a <+> b = a + b

main :: IO ()
main = print (1 <+> 2)
