-- Haskell 2010 Report 4.3.1: a constructor class, whose class variable has kind * -> *
module Main (main) where

class Container f where
  empty :: f a
  insert :: a -> f a -> f a
  toL :: f a -> [a]

newtype Stack a = Stack [a]

newtype Queue a = Queue [a]

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Container [] where
  empty = []
  insert = (:)
  toL xs = xs

instance Container Stack where
  empty = Stack []
  insert x (Stack xs) = Stack (x : xs)
  toL (Stack xs) = xs

instance Container Queue where
  empty = Queue []
  insert x (Queue xs) = Queue (xs ++ [x])
  toL (Queue xs) = xs

instance Container Maybe where
  empty = Nothing
  insert x _ = Just x
  toL Nothing = []
  toL (Just x) = [x]

-- inserts at the leftmost position
instance Container Tree where
  empty = Leaf
  insert x Leaf = Node Leaf x Leaf
  insert x (Node l v r) = Node (insert x l) v r
  toL Leaf = []
  toL (Node l v r) = toL l ++ [v] ++ toL r

fill :: Container f => [a] -> f a
fill = foldr insert empty

transfer :: (Container f, Container g) => f a -> g a
transfer = fill . toL

size :: Container f => f a -> Int
size = length . toL

main :: IO ()
main = do
  print ("list", toL (fill [1, 2, 3] :: [Int]))
  print ("stack", toL (fill [1, 2, 3] :: Stack Int))
  print ("queue", toL (fill [1, 2, 3] :: Queue Int))
  print ("maybe", toL (fill "abc" :: Maybe Char))
  print ("tree", toL (fill "abc" :: Tree Char))
  print ("empty", toL (empty :: Stack Bool), toL (empty :: Maybe Bool))
  print ("stack to queue", toL (transfer (Stack "xyz") :: Queue Char))
  print ("queue to maybe", toL (transfer (Queue [True, False]) :: Maybe Bool))
  print ("size", size (Stack "four"), size (Just 'x'), size (fill [1, 2 :: Int] :: Tree Int))
  print ("insert twice", toL (insert 'a' (insert 'b' empty) :: Queue Char))
