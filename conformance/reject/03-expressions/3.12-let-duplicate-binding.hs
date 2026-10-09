-- Haskell 2010 Report 3.12 / 4.4.3 (REJECT): a let may not bind the same variable twice in one declaration group
module Main (main) where

main :: IO ()
main = print (let { x = 1 :: Int; y = 2; x = 3 } in x + y)
