-- Haskell 2010 Report 20.6 (Data.List): searching by equality (elem, notElem, lookup) and with a predicate
-- (find, filter, partition)
module Main (main) where

import Data.List

table :: [(Int, String)]
table = [(1, "one"), (2, "two"), (3, "three"), (2, "zwei")]

main :: IO ()
main = do
  print ("elem", 3 `elem` [1, 2, 3 :: Int], 4 `elem` [1, 2, 3 :: Int])
  print ("elem empty", 'a' `elem` "")
  print ("elem infinite", 50 `elem` [1 :: Int ..])
  print ("elem stops at first match", 1 `elem` (1 : undefined :: [Int]))
  print ("elem of lists", "b" `elem` ["a", "b"], [] `elem` [[1 :: Int]])
  print ("notElem", 3 `notElem` [1, 2, 3 :: Int], 4 `notElem` [1, 2, 3 :: Int])
  print ("notElem empty", 'a' `notElem` "")
  print ("notElem is not elem", all (\c -> notElem c "hello" == not (elem c "hello")) "abcdefghijklmno")
  print ("elem fixity is infix 4", 1 `elem` [1 :: Int] && 2 `notElem` [1 :: Int])
  print ("elem with arithmetic", 1 + 1 `elem` [2 :: Int])
  print ("lookup", lookup 1 table, lookup 3 table)
  print ("lookup missing", lookup 9 table)
  print ("lookup first of duplicates", lookup 2 table)
  print ("lookup empty", lookup 'x' ([] :: [(Char, Int)]))
  print ("lookup infinite", lookup 5 (zip [1 :: Int ..] (cycle "abc")))
  print ("lookup string keys", lookup "b" [("a", 1), ("b", 2 :: Int)])
  print ("find", find (> 3) [1, 5, 2, 7 :: Int])
  print ("find none", find (> 9) [1, 5, 2, 7 :: Int])
  print ("find empty", find even ([] :: [Int]))
  print ("find first", find even [1, 4, 6 :: Int])
  print ("find infinite", find (\x -> x * x > 200) [1 :: Int ..])
  print ("find is listToMaybe of filter", find odd [2, 4, 5, 7 :: Int] == (case filter odd [2, 4, 5, 7] of { [] -> Nothing; (y : _) -> Just y }))
  print ("filter", filter even [1 .. 10 :: Int])
  print ("filter none", filter (> 10) [1 .. 10 :: Int])
  print ("filter all", filter (> 0) [1, 2, 3 :: Int])
  print ("filter empty", filter even ([] :: [Int]))
  print ("filter keeps order and duplicates", filter (/= ' ') "a b  a c")
  print ("filter infinite", take 4 (filter ((== 0) . (`mod` 7)) [1 :: Int ..]))
  print ("filter twice", filter even (filter (> 3) [1 .. 10 :: Int]) == filter (\x -> even x && x > 3) [1 .. 10])
  print ("partition", partition even [1 .. 10 :: Int])
  print ("partition all true", partition (> 0) [1, 2, 3 :: Int])
  print ("partition all false", partition (< 0) [1, 2, 3 :: Int])
  print ("partition empty", partition even ([] :: [Int]))
  print ("partition string", partition (`elem` "aeiou") "the quick brown fox")
  print ("partition is two filters", partition odd [3, 1, 4, 1, 5 :: Int] == (filter odd [3, 1, 4, 1, 5], filter (not . odd) [3, 1, 4, 1, 5]))
  print ("partition infinite", take 3 (fst (partition even [1 :: Int ..])), take 3 (snd (partition even [1 :: Int ..])))
