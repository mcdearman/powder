-- Haskell 2010 Report 20.2 (Data.List): subsequences and permutations, including the order of the results
-- (subsequences "abc" == ["","a","b","ab","c","ac","bc","abc"],
--  permutations "abc" == ["abc","bac","cba","bca","cab","acb"])
module Main (main) where

import Data.List

main :: IO ()
main = do
  print ("subsequences abc", subsequences "abc")
  print ("subsequences empty", subsequences "")
  print ("subsequences singleton", subsequences "a")
  print ("subsequences two", subsequences [1, 2 :: Int])
  print ("subsequences four", subsequences [1, 2, 3, 4 :: Int])
  print ("subsequences with duplicates", subsequences "aab")
  print ("subsequences count", map (length . subsequences . flip take "abcdefghij") [0 .. 10])
  print ("subsequences preserve order", all (\s -> s == filter (`elem` s) "abcde") (subsequences "abcde"))
  print ("subsequences of infinite list", take 8 (subsequences [1 :: Int ..]))
  print ("subsequences prefix property", take 8 (subsequences "abcdef") == subsequences "abc")
  print ("subsequences sizes", map length (subsequences "abcd"))
  print ("permutations abc", permutations "abc")
  print ("permutations empty", permutations "")
  print ("permutations singleton", permutations "a")
  print ("permutations two", permutations [1, 2 :: Int])
  print ("permutations four", permutations [1, 2, 3, 4 :: Int])
  print ("permutations with duplicates", permutations "aab")
  print ("permutations count", map (length . permutations . flip take "abcdef") [0 .. 6])
  print ("permutations first is identity", head (permutations "abcdef"))
  print ("permutations are distinct", length (nub (permutations "abcd")))
  print ("permutations sorted", sort (permutations [1, 2, 3 :: Int]))
  print ("each is a permutation", all (\p -> sort p == "abcd") (permutations "abcd"))
