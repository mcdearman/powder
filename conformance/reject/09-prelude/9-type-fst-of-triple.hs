-- Haskell 2010 Report 9: REJECT. fst :: (a, b) -> a applies only to pairs, not to triples.
module Main (main) where

main :: IO ()
main = print (fst (1 :: Int, 2 :: Int, 3 :: Int))
