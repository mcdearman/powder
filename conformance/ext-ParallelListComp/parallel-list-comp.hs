{-# LANGUAGE ParallelListComp #-}
-- ParallelListComp: qualifiers after a second bar are gone through beside the first, as far as the shorter goes
module Main (main) where

main :: IO ()
main = do
  print [(x, y) | x <- [1, 2, 3 :: Int] | y <- "abc"]
  print [x + y | x <- [1, 2, 3 :: Int] | y <- [10, 20 :: Int]]
  print [(x, y, z) | x <- [1 :: Int ..] | y <- "ab" | z <- [True, False, True]]
  -- Each row has qualifiers of its own.
  print [(x, y) | x <- [1 .. 10 :: Int], even x | y <- "abcdef", y /= 'b']
  print [(a, b, c) | a <- [1, 2 :: Int], b <- "xy" | c <- [0 :: Int ..]]
  print [x * y | (x, _) <- [(1, 'a'), (2, 'b'), (3 :: Int, 'c')] | Just y <- [Just 1, Nothing, Just 2, Just (3 :: Int)]]
  print [(x, y) | x <- [1, 2, 3 :: Int], let x' = x * 10 | y <- [x' | x' <- "pqr"]]
  print [s + t | x <- [1, 2, 3 :: Int], let s = x * x | t <- [100, 200, 300 :: Int]]
  print [() | _ <- [1, 2, 3 :: Int] | _ <- "ab"]
  print (take 5 [(n, f) | n <- [0 :: Int ..] | f <- fibs])
  where
    fibs = 0 : 1 : [a + b | a <- fibs | b <- tail fibs] :: [Integer]
