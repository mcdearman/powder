-- Haskell 2010 Report 3.15.2 (REJECT): a field label may not be mentioned more than once in a construction
module Main (main) where

data P = P { px :: Int, py :: Int }

main :: IO ()
main = print (px (P { px = 1, py = 2, px = 3 }))
