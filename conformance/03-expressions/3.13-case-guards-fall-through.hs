-- Haskell 2010 Report 3.13: if every guard of an alternative fails, matching continues with the next alternative
module Main (main) where

g :: (Int, Int) -> String
g p = case p of
  (x, y) | x > y -> "gt"
         | x == y, x > 10 -> "eq big"
  (0, _) -> "zero first"
  (x, _) | x < 0 -> "neg first"
  _ -> "other"

h :: Maybe Int -> Int -> String
h m k = case m of
  Just n | n > k -> "above"
  Just n | n == k -> "equal"
         | even n -> "below even"
  Nothing | k > 0 -> "none pos"
  _ | k < 0 -> "neg k"
    | otherwise -> "fallback"

firstTrue :: Int -> Int
firstTrue n = case n of
  _ | n > 0 -> 1
    | n > 10 -> 2
    | otherwise -> 3

main :: IO ()
main = do
  print ("gt", g (5, 1))
  print ("eq big", g (11, 11))
  print ("eq small falls to zero", g (0, 0))
  print ("zero first", g (0, 5))
  print ("neg first", g (-1, 3))
  print ("other", g (1, 2), g (3, 3))
  print ("h just", h (Just 5) 3, h (Just 3) 3, h (Just 2) 3, h (Just 1) 3)
  print ("h nothing", h Nothing 1, h Nothing 0, h Nothing (-1))
  print ("h just neg k", h (Just (-3)) (-1))
  print ("first true guard wins", map firstTrue [20, 5, 0])
  print ("guard binds pattern vars", case (3, 4) of { (a, b) | a + b == 7 -> a * b; _ -> 0 })
  print ("guard false never evaluates body", case 1 of { n | n > 5 -> undefined; _ -> "safe" })
  print ("guards on literal alt", case 'a' of { 'a' | False -> 1; 'a' | True -> 2; _ -> 3 })
  print ("otherwise is True", otherwise, case () of { _ | otherwise -> "ok" })
  print ("guard with let", case 10 of { n | let m = n * n, m > 50 -> m; _ -> 0 })
  print ("pattern guard", case Just 3 of { v | Just n <- v, n > 2 -> n; _ -> 0 })
