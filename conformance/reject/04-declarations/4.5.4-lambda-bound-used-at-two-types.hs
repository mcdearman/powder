-- Haskell 2010 Report 4.5.4: a lambda-bound variable is monomorphic, so `i` cannot be applied to both a Char and a Bool
module Main (main) where

main :: IO ()
main = print ((\i -> (i 'a', i True)) id)
