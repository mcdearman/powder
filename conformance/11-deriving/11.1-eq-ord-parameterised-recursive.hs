-- Haskell 2010 Report 11.1: derived Eq and Ord for parameterised and recursive types (Maybe-like, Either-like, list, tree)
module Main (main) where

data Opt a = None | Some a deriving (Eq, Ord, Show)

data Alt a b = L a | R b deriving (Eq, Ord, Show)

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show)

fromL :: [a] -> List a
fromL = foldr Cons Nil

toOpt :: Maybe a -> Opt a
toOpt = maybe None Some

leaf :: a -> Tree a
leaf x = Node Leaf x Leaf

strings :: [String]
strings = ["", "a", "ab", "abc", "b", "ba", "aa"]

maybes :: [Maybe Int]
maybes = [Nothing, Just (-1), Just 0, Just 5]

main :: IO ()
main = do
  print ("Opt Eq", Some 1 == Some (1 :: Int), Some 1 == Some (2 :: Int), Some 'a' == None, (None :: Opt Int) == None)
  print ("Opt Ord", None < Some (minBound :: Int), compare (Some 2) (Some (1 :: Int)), compare (None :: Opt Char) None, max None (Some 'a'))
  print ("Opt agrees with Maybe", and [compare (toOpt a) (toOpt b) == compare a b && (toOpt a == toOpt b) == (a == b) | a <- maybes, b <- maybes])
  print ("Opt nested", Some (Some 1) > Some (None :: Opt Int), Some None > (None :: Opt (Opt Int)), Some (Some 'a') == Some (Some 'a'))
  print ("Alt Eq", (L 1 :: Alt Int Int) == L 1, (L 1 :: Alt Int Int) == R 1, (R 'x' :: Alt Int Char) == R 'x')
  print ("Alt Ord", (L 99 :: Alt Int Int) < R 0, compare (R 'a' :: Alt Int Char) (L 5), compare (R 'a' :: Alt Int Char) (R 'b'), min (L "z") (R 'a'))
  print ("List Eq", fromL "abc" == fromL "abc", fromL "abc" == fromL "abd", fromL "ab" == fromL "abc", fromL "" == (Nil :: List Char))
  print ("List Ord agrees with []", and [compare (fromL a) (fromL b) == compare a b | a <- strings, b <- strings])
  print ("List prefix is smaller", fromL "ab" < fromL "abc", Nil < fromL [0 :: Int], compare (fromL [1, 2, 3 :: Int]) (fromL [1, 2]))
  print ("List first difference decides", fromL [1, 9, 9 :: Int] < fromL [2], compare (fromL [1, 2, 3 :: Int]) (fromL [1, 3, 0]))
  print ("List of structures", fromL [Some 1, None] < fromL [Some (1 :: Int), Some 0], fromL [fromL "a"] == fromL [fromL "a"])
  print ("Tree Eq", leaf 'a' == leaf 'a', leaf 'a' == leaf 'b', leaf 'a' == Leaf, Node (leaf 1) 2 (leaf 3) == Node (leaf 1) 2 (leaf (3 :: Int)))
  print ("Tree Ord: Leaf first", Leaf < leaf 'a', compare (leaf 'a') Leaf, compare (Leaf :: Tree Int) Leaf)
  print ("Tree Ord: left subtree first", compare (Node Leaf 'z' Leaf) (Node (leaf 'a') 'a' Leaf), compare (Node (leaf 'b') 'a' Leaf) (Node (leaf 'a') 'z' (leaf 'z')))
  print ("Tree Ord: then value", compare (Node (leaf 1) 5 (leaf 9)) (Node (leaf 1) 6 Leaf), compare (Node Leaf 2 Leaf) (Node Leaf (1 :: Int) (leaf 9)))
  print ("Tree Ord: then right subtree", compare (Node Leaf 1 Leaf) (Node Leaf 1 (leaf (0 :: Int))), compare (Node Leaf 1 (leaf 2)) (Node Leaf 1 (leaf (1 :: Int))))
  print ("Tree of Opt", leaf (Some 'a') > leaf None, maximum [leaf (Some 1), Leaf, leaf (Some (3 :: Int)), leaf None])
  print ("long lists", fromL [1 .. 5000 :: Int] == fromL [1 .. 5000], fromL [1 .. 5000 :: Int] < fromL ([1 .. 4999] ++ [5001]))
