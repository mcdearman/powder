-- Haskell 2010 Report 3.5 (REJECT): (op e) is legal only if (x op e) parses as x op (e); here x * 2 + 1 is (x * 2) + 1
module Main (main) where

main :: IO ()
main = print ((* 2 + 1) (3 :: Int))
