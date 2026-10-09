-- Haskell 2010 Report 20.4.2 (Data.List): accumulating maps mapAccumL (left to right) and mapAccumR
-- (right to left)
module Main (main) where

import Data.List

step :: Int -> Int -> (Int, Int)
step acc x = (acc + x, acc * x)

number :: Int -> String -> (Int, String)
number n s = (n + 1, show n ++ ":" ++ s)

trace :: String -> Char -> (String, String)
trace seen c = (seen ++ [c], seen)

main :: IO ()
main = do
  print ("mapAccumL", mapAccumL step 0 [1, 2, 3])
  print ("mapAccumR", mapAccumR step 0 [1, 2, 3])
  print ("mapAccumL empty", mapAccumL step 5 [])
  print ("mapAccumR empty", mapAccumR step 5 [])
  print ("mapAccumL singleton", mapAccumL step 5 [4])
  print ("mapAccumR singleton", mapAccumR step 5 [4])
  print ("mapAccumL numbering", mapAccumL number 1 ["a", "b", "c"])
  print ("mapAccumR numbering", mapAccumR number 1 ["a", "b", "c"])
  print ("mapAccumL order of visits", mapAccumL trace "" "abc")
  print ("mapAccumR order of visits", mapAccumR trace "" "abc")
  print ("mapAccumL running sum", snd (mapAccumL (\a x -> (a + x, a + x)) 0 [1, 2, 3, 4 :: Int]))
  print ("mapAccumR suffix sum", snd (mapAccumR (\a x -> (a + x, a + x)) 0 [1, 2, 3, 4 :: Int]))
  print ("mapAccumL final accumulator is foldl", fst (mapAccumL (\a x -> (a - x, ())) 100 [1, 2, 3 :: Int]) == foldl (-) 100 [1, 2, 3])
  print ("mapAccumR final accumulator is foldr flipped", fst (mapAccumR (\a x -> (x - a, ())) 100 [1, 2, 3 :: Int]) == foldr (-) 100 [1, 2, 3])
  print ("mapAccumL as map", snd (mapAccumL (\a x -> (a, x * 2)) () [1, 2, 3 :: Int]))
  print ("mapAccumL result list is lazy", take 3 (snd (mapAccumL (\a x -> (a + 1, a * x)) 1 [1 :: Int ..])))
  print ("mapAccumL carry digits", mapAccumL (\carry d -> ((d * 2 + carry) `div` 10, (d * 2 + carry) `mod` 10)) 0 [7, 8, 9 :: Int])
