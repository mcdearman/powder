-- Haskell 2010 Report 20.9.3 (Data.List): ordered lists sort (stable) and insert (also into unsorted lists)
module Main (main) where

import Data.List

-- compared by key only, so equal keys expose the stability of sort
data Keyed = Keyed Int Char
  deriving (Show)

instance Eq Keyed where
  Keyed a _ == Keyed b _ = a == b

instance Ord Keyed where
  compare (Keyed a _) (Keyed b _) = compare a b

ks :: [Keyed]
ks = [Keyed 2 'a', Keyed 1 'b', Keyed 2 'c', Keyed 1 'd', Keyed 3 'e', Keyed 2 'f', Keyed 1 'g']

main :: IO ()
main = do
  print ("sort", sort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3 :: Int])
  print ("sort empty", sort ([] :: [Int]))
  print ("sort singleton", sort [7 :: Int])
  print ("sort already sorted", sort [1, 2, 3, 4 :: Int])
  print ("sort reversed", sort [4, 3, 2, 1 :: Int])
  print ("sort all equal", sort [2, 2, 2 :: Int])
  print ("sort negatives", sort [0, -5, 3, -1, 2 :: Int])
  print ("sort string", sort "the quick brown fox")
  print ("sort strings", sort ["banana", "Apple", "cherry", "apple", "", "b"])
  print ("sort pairs", sort [(2, 'a'), (1, 'z'), (2, 'A'), (1, 'a') :: (Int, Char)])
  print ("sort lists", sort [[2], [1, 2], [], [1], [1, 1, 5 :: Int]])
  print ("sort Bool Ordering", sort [True, False, True], sort [GT, LT, EQ, LT])
  print ("sort Maybe", sort [Just 3, Nothing, Just 1 :: Maybe Int])
  print ("sort Double", sort [2.5, -0.5, 1.0e10, 1.0e-3 :: Double])
  print ("sort large", sort (reverse [1 .. 1000 :: Int]) == [1 .. 1000])
  print ("sort pseudo random", take 10 (sort [(i * 7919) `mod` 1009 | i <- [1 .. 500 :: Int]]))
  print ("sort idempotent", sort (sort "idempotent") == sort "idempotent")
  print ("sort keeps length and elements", sort "mississippi", length (sort "mississippi"))
  print ("sort is stable", sort ks)
  print ("sort stable on all equal keys", sort [Keyed 0 'x', Keyed 0 'y', Keyed 0 'z'])
  print ("insert", insert 3 [1, 2, 4, 5 :: Int])
  print ("insert at front", insert 0 [1, 2, 3 :: Int])
  print ("insert at end", insert 9 [1, 2, 3 :: Int])
  print ("insert into empty", insert 'x' "")
  print ("insert duplicate", insert 2 [1, 2, 2, 3 :: Int])
  print ("insert before equal elements", insert (Keyed 2 'n') [Keyed 1 'a', Keyed 2 'b', Keyed 2 'c', Keyed 3 'd'])
  print ("insert into unsorted stops at first greater or equal", insert 3 [5, 1, 4 :: Int])
  print ("insert into unsorted skips smaller prefix", insert 4 [1, 5, 2 :: Int])
  print ("insert into descending", insert 3 [5, 4, 2, 1 :: Int], insert 6 [5, 4, 2, 1 :: Int])
  print ("insert into infinite", take 5 (insert 3 [1, 3 ..] :: [Int]))
  print ("insertion sort", foldr insert [] [3, 1, 4, 1, 5, 9, 2, 6 :: Int])
  print ("insertion sort agrees with sort", foldr insert [] "hello world" == sort "hello world")
