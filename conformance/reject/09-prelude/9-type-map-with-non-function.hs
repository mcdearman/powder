-- Haskell 2010 Report 9: REJECT. The first argument of map :: (a -> b) -> [a] -> [b] must be a function, not an Int.
module Main (main) where

main :: IO ()
main = print (map (1 :: Int) [2 :: Int] :: [Int])
