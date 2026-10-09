-- Haskell 2010 Report 10.6 / 3.4: unary minus has precedence 6, so it may not follow an operator of precedence >= 6 (a * -b)
module Main (main) where

main :: IO ()
main = print (2 * -3 :: Int)
