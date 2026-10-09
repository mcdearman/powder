-- Haskell 2010 Report 9: REJECT. `sort` is not exported by the Prelude (it lives in Data.List, which is not imported).
module Main (main) where

main :: IO ()
main = print (sort [3, 1, 2 :: Int])
