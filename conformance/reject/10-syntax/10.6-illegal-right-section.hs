-- Haskell 2010 Report 3.5: (op e) is legal only if (x op e) parses as (x op (e)); (* 2 + 1) is not, since * binds tighter than +
module Main (main) where

main :: IO ()
main = print ((* 2 + 1) (3 :: Int))
