-- Haskell 2010 Report 3.4 / 10.6 (REJECT): prefix minus has precedence 6 and cannot directly follow an operator of precedence 6 or higher
module Main (main) where

main :: IO ()
main = print (1 + -2 :: Int)
