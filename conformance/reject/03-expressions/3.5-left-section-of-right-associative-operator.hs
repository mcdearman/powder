-- Haskell 2010 Report 3.5 (REJECT): (e op) is legal only if (e op x) parses as (e) op x; ++ is infixr, so (a ++ b ++) is illegal
module Main (main) where

main :: IO ()
main = print (("a" ++ "b" ++) "c")
