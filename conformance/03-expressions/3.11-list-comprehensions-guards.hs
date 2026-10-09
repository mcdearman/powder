-- Haskell 2010 Report 3.11: boolean guards in list comprehensions
module Main (main) where

main :: IO ()
main = do
  print ("one guard", [x | x <- [1 .. 10], even x])
  print ("two guards", [x | x <- [1 .. 20], x > 5, x `mod` 3 == 0])
  print ("guard between generators", [(x, y) | x <- [1 .. 4], odd x, y <- "ab"])
  print ("guard on both", [(x, y) | x <- [1 .. 3], y <- [1 .. 3], x < y])
  print ("guard only true", [1 | True])
  print ("guard only false", ['x' | False])
  print ("guard first", [x | 1 < 2, x <- "ab"], [x | 1 > 2, x <- "ab"])
  print ("two guards only", [() | True, True], [() | True, False])
  print ("guard uses outer", let limit = 3 in [x | x <- [1 .. 6], x <= limit])
  print ("guard with function", [w | w <- ["a", "bb", "ccc"], length w >= 2])
  print ("always false", [x | x <- [1, 2, 3], False])
  print ("otherwise", [x | x <- [1, 2, 3], otherwise])
  -- a failed guard skips the later generators entirely, so undefined is never demanded
  print ("guard short-circuits", [y | x <- [1, 2], odd x, y <- if x == 1 then [10] else undefined])
  print ("factors", [d | d <- [1 .. 28], 28 `mod` d == 0])
  print ("primes", [n | n <- [2 .. 30], null [d | d <- [2 .. n - 1], n `mod` d == 0]])
  print ("guard before use", [10 `div` x | x <- [0, 1, 2, 5], x /= 0])
  print ("complex condition", [x | x <- [1 .. 12], even x && x > 4 || x == 1])
  print ("if in guard", [x | x <- [1 .. 6], if x < 3 then True else x == 6])
