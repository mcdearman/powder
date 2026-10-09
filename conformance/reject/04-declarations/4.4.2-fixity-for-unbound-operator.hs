-- Haskell 2010 Report 4.4.2: a fixity declaration must be in the same declaration list as the definition of its operator
module Main (main) where

infixl 6 <+>

main :: IO ()
main = putStrLn "unreachable"
