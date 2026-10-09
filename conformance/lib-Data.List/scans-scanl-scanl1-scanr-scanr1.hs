-- Haskell 2010 Report 20.4.1 (Data.List): scans scanl, scanl1, scanr, scanr1
module Main (main) where

import Data.List

xs :: [Int]
xs = [1, 2, 3, 4]

fibs :: [Integer]
fibs = 0 : scanl (+) 1 fibs

main :: IO ()
main = do
  print ("scanl", scanl (+) 0 xs)
  print ("scanl minus", scanl (-) 100 xs)
  print ("scanl empty", scanl (+) 0 ([] :: [Int]))
  print ("scanl snoc", scanl (flip (:)) "" "abc")
  print ("scanl different types", scanl (\n s -> n + length s) 0 ["ab", "", "cde"])
  print ("scanl infinite", take 6 (scanl (+) 0 [1 :: Int ..]))
  print ("scanl first element needs no list", head (scanl (+) 5 (undefined :: [Int])))
  print ("scanl length", length (scanl (+) 0 xs) == length xs + 1)
  print ("scanl last is foldl", last (scanl (-) 100 xs) == foldl (-) 100 xs)
  print ("fibs", take 15 fibs)
  print ("scanl1", scanl1 (+) xs)
  print ("scanl1 minus", scanl1 (-) xs)
  print ("scanl1 empty", scanl1 (+) ([] :: [Int]))
  print ("scanl1 singleton", scanl1 (+) [9 :: Int])
  print ("scanl1 running maximum", scanl1 max [3, 1, 4, 1, 5, 9, 2, 6 :: Int])
  print ("scanl1 infinite", take 5 (scanl1 (*) [1 :: Integer ..]))
  print ("scanr", scanr (+) 0 xs)
  print ("scanr minus", scanr (-) 100 xs)
  print ("scanr empty", scanr (+) 0 ([] :: [Int]))
  print ("scanr cons", scanr (:) "" "abc")
  print ("scanr different types", scanr (\s n -> n + length s) 0 ["ab", "", "cde"])
  print ("scanr length", length (scanr (+) 0 xs) == length xs + 1)
  print ("scanr head is foldr", head (scanr (-) 100 xs) == foldr (-) 100 xs)
  print ("scanr last is seed", last (scanr (+) 42 xs))
  print ("scanr1", scanr1 (+) xs)
  print ("scanr1 minus", scanr1 (-) xs)
  print ("scanr1 empty", scanr1 (+) ([] :: [Int]))
  print ("scanr1 singleton", scanr1 (+) [9 :: Int])
  print ("scanr1 suffix maximum", scanr1 max [3, 1, 4, 1, 5, 9, 2, 6 :: Int])
  print ("scanr1 head is foldr1", head (scanr1 (-) xs) == foldr1 (-) xs)
  print ("scanl is map foldl over inits", scanl (-) 100 xs == map (foldl (-) 100) (inits xs))
  print ("scanr is map foldr over tails", scanr (-) 100 xs == map (foldr (-) 100) (tails xs))
