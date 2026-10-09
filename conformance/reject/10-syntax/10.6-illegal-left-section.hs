-- Haskell 2010 Report 3.5: (e op) is legal only if (e op x) parses as ((e) op x); (1 + 2 *) is not, since * binds tighter than +
module Main (main) where

main :: IO ()
main = print ((1 + 2 *) (3 :: Int))
