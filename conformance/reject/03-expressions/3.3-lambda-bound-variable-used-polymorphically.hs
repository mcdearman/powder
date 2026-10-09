-- Haskell 2010 Report 3.3 / 4.5.4 (REJECT): a lambda-bound variable is monomorphic and cannot be used at two types
module Main (main) where

main :: IO ()
main = print ((\f -> (f 'a', f True)) id)
