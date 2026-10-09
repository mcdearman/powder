-- Haskell 2010 Report 3.17.1 (REJECT): patterns are linear; no variable may appear more than once
module Main (main) where

same :: Int -> Int -> Bool
same x x = True

main :: IO ()
main = print (same 1 1)
