-- Haskell 2010 Report 4.5.2: a definition without a signature is generalised, and can be given two
-- different, more specific types
module Main (main) where

poly x y = y x

specific1 :: Int -> (Int -> a) -> a
specific1 = poly

specific2 :: Char -> (Char -> a) -> a
specific2 = poly

pair x = (x, x)

main :: IO ()
main = do
  print (specific1 3 (+ 1), specific1 3 show)
  print (specific2 'c' succ, specific2 'c' fromEnum)
  print (poly "s" length, poly True not)
  print (pair 'x', pair (pair ()), fst (pair [1 :: Int]))
