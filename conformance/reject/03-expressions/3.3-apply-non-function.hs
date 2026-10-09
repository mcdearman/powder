-- Haskell 2010 Report 3.3 (REJECT): only a value of function type can be applied to an argument
module Main (main) where

flag :: Bool
flag = True

main :: IO ()
main = print (flag 'x')
