-- Haskell 2010 Report 20.10.1 (Data.List): user-supplied equality: nubBy, deleteBy, deleteFirstsBy, unionBy,
-- intersectBy, groupBy
module Main (main) where

import Data.Char (toLower)
import Data.List

sameCase :: Char -> Char -> Bool
sameCase a b = toLower a == toLower b

mod3 :: Int -> Int -> Bool
mod3 a b = a `mod` 3 == b `mod` 3

main :: IO ()
main = do
  print ("nubBy", nubBy sameCase "aAbBAbcC")
  print ("nubBy mod", nubBy mod3 [1 .. 10])
  print ("nubBy empty", nubBy mod3 [])
  print ("nubBy (==) is nub", nubBy (==) "mississippi" == nub "mississippi")
  print ("nubBy always equal", nubBy (\_ _ -> True) "abc")
  print ("nubBy never equal", nubBy (\_ _ -> False) "aaa")
  print ("nubBy passes the earlier element first", nubBy (\x y -> y `mod` x == 0) [2 .. 30 :: Int])
  print ("deleteBy", deleteBy sameCase 'B' "abcb")
  print ("deleteBy missing", deleteBy sameCase 'z' "abc")
  print ("deleteBy only first", deleteBy mod3 4 [2, 1, 3, 7, 10])
  print ("deleteBy empty", deleteBy mod3 4 [])
  print ("deleteBy passes the given element first", deleteBy (<=) 3 [5, 1, 4 :: Int], deleteBy (<=) 3 [1, 2, 3, 4 :: Int])
  print ("deleteBy (==) is delete", deleteBy (==) 'a' "banana" == delete 'a' "banana")
  print ("deleteFirstsBy", deleteFirstsBy sameCase "aAbBcC" "ab")
  print ("deleteFirstsBy duplicates", deleteFirstsBy sameCase "aAaA" "Aa")
  print ("deleteFirstsBy nothing to delete", deleteFirstsBy sameCase "abc" "")
  print ("deleteFirstsBy absent", deleteFirstsBy sameCase "abc" "xyz")
  print ("deleteFirstsBy from empty", deleteFirstsBy sameCase "" "abc")
  print ("deleteFirstsBy (==) is difference", deleteFirstsBy (==) "abcabc" "cb" == "abcabc" \\ "cb")
  print ("unionBy", unionBy sameCase "abC" "cDdA")
  print ("unionBy mod", unionBy mod3 [1, 2] [4, 6, 9, 5])
  print ("unionBy keeps duplicates of first", unionBy sameCase "aA" "b")
  print ("unionBy empty first", unionBy sameCase "" "aAbB")
  print ("unionBy empty second", unionBy sameCase "aAbB" "")
  print ("unionBy (==) is union", unionBy (==) "abca" "cdd" == union "abca" "cdd")
  print ("intersectBy", intersectBy sameCase "aBcD" "CAx")
  print ("intersectBy keeps duplicates of first", intersectBy sameCase "aAbB" "a")
  print ("intersectBy disjoint", intersectBy sameCase "abc" "xyz")
  print ("intersectBy empty", intersectBy sameCase "" "abc", intersectBy sameCase "abc" "")
  print ("intersectBy passes first-list element first", intersectBy (\a b -> a == b * 2) [2, 4, 5, 6] [1, 2, 3 :: Int])
  print ("intersectBy (==) is intersect", intersectBy (==) "abca" "ca" == intersect "abca" "ca")
  print ("groupBy", groupBy sameCase "aAabBAcC")
  print ("groupBy parity", groupBy (\a b -> even a == even b) [2, 4, 1, 3, 5, 6, 7 :: Int])
  print ("groupBy empty", groupBy mod3 [])
  print ("groupBy compares with first of group", groupBy (<=) [1, 2, 2, 3, 1, 2, 0, 4, 5, 2 :: Int])
  print ("groupBy (==) is group", groupBy (==) "Mississippi" == group "Mississippi")
  print ("groupBy always", groupBy (\_ _ -> True) "abc")
  print ("groupBy never", groupBy (\_ _ -> False) "aaa")
  print ("groupBy on keys", map (map snd) (groupBy (\a b -> fst a == fst b) [(1 :: Int, 'a'), (1, 'b'), (2, 'c'), (1, 'd')]))
  print ("groupBy words", filter (not . all (== ' ')) (groupBy (\a b -> (a == ' ') == (b == ' ')) "split  these words"))
