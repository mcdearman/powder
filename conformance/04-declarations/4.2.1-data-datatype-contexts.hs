-- Haskell 2010 Report 4.2.1: datatype contexts (data Eq a => Set a = ...); GHC -XHaskell2010 enables DatatypeContexts
module Main (main) where

data Eq a => Set a = NilSet | ConsSet a (Set a)

data (Ord k, Show v) => Entry k v = Entry k v

newtype Ord a => Sorted a = Sorted [a]

emptySet :: Eq a => Set a
emptySet = NilSet

memberSet :: Eq a => a -> Set a -> Bool
memberSet _ NilSet = False
memberSet x (ConsSet y rest) = x == y || memberSet x rest

insertSet :: Eq a => a -> Set a -> Set a
insertSet x s = if memberSet x s then s else ConsSet x s

toListSet :: Eq a => Set a -> [a]
toListSet NilSet = []
toListSet (ConsSet x rest) = x : toListSet rest

fromListSet :: Eq a => [a] -> Set a
fromListSet = foldr insertSet emptySet

describe :: (Ord k, Show v) => Entry k v -> Entry k v -> String
describe (Entry k1 v1) (Entry k2 v2)
  | k1 <= k2 = show v1 ++ "<=" ++ show v2
  | otherwise = show v1 ++ ">" ++ show v2

insertSorted :: Ord a => a -> Sorted a -> Sorted a
insertSorted x (Sorted xs) = Sorted (takeWhile (< x) xs ++ [x] ++ dropWhile (< x) xs)

unSorted :: Ord a => Sorted a -> [a]
unSorted (Sorted xs) = xs

main :: IO ()
main = do
  print ("set", toListSet (fromListSet [1, 2, 1, 3, 2 :: Int]))
  print ("set chars", toListSet (fromListSet "mississippi"))
  print ("set empty", toListSet (emptySet :: Set Bool))
  print ("member", memberSet 'a' (fromListSet "abc"), memberSet 'z' (fromListSet "abc"))
  print ("constructor", toListSet (ConsSet True (ConsSet False NilSet)))
  print ("entry", describe (Entry 'a' True) (Entry 'b' False))
  print ("entry gt", describe (Entry (2 :: Int) "x") (Entry 1 "y"))
  print ("sorted", unSorted (foldr insertSorted (Sorted []) [3, 1, 2 :: Int]))
