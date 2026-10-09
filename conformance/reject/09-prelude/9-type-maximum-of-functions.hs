-- Haskell 2010 Report 9: REJECT. maximum :: Ord a => [a] -> a needs an Ord instance for the element type, and
-- functions have none.
module Main (main) where

main :: IO ()
main = print (maximum [negate, abs :: Int -> Int] 3)
