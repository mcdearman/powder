{-# LANGUAGE MultiWayIf #-}
-- MultiWayIf: an if with guards, the first that holds
module Main (main) where

size :: Int -> String
size n =
  if | n < 0 -> "negative"
     | n == 0 -> "zero"
     | n < 10, even n -> "small and even"
     | n < 10 -> "small"
     | otherwise -> "large"

main :: IO ()
main = do
  print (map size [-1, 0, 4, 7, 100])
  print (if | Just x <- lookup 'b' table -> x | otherwise -> 0)
  print [(if | even n -> n `div` 2 | otherwise -> 3 * n + 1) | n <- [1 .. 6 :: Int]]
  let nested x y = if | x -> (if | y -> "both" | otherwise -> "first") | y -> "second" | otherwise -> "neither"
  print [nested a b | a <- [True, False], b <- [True, False]]
  print (1 + (if | True -> 2 | otherwise -> 3) * 4 :: Int)
  where
    table = [('a', 1), ('b', 2 :: Int)]
