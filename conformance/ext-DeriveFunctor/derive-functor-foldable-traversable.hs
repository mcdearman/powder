{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- DeriveFunctor, DeriveFoldable, DeriveTraversable: instances over a type's last parameter, worked out from its fields
module Main (main) where

import Data.Foldable (toList)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

data Pair a = Pair a a deriving (Show, Functor, Foldable, Traversable)

-- The last parameter is the one gone over, and the others are left alone.
data Tagged t a = Tagged t a | Untagged a deriving (Show, Functor, Foldable, Traversable)

-- Inside lists, tuples, Maybe and other functors.
data Nested a = Nested [a] (Maybe a) (Int, a) [(Char, [a])] deriving (Show, Functor, Foldable, Traversable)

data Rose a = Rose a [Rose a] deriving (Show, Functor, Foldable, Traversable)

-- A field that is a function is mapped over what it gives.
newtype Reader r a = Reader (r -> a) deriving (Functor)

-- A parameter that is itself a functor.
data Wrap f a = Wrap (f a) | Both (f a) (f (f a)) deriving (Functor, Foldable, Traversable)

data Record a = Record {first :: a, count :: Int, rest :: [a]} deriving (Show, Functor, Foldable, Traversable)

data Sum' a b = L a | R b | b :*: b deriving (Show, Functor, Foldable, Traversable)

data Phantom a = Phantom Int deriving (Show, Functor, Foldable, Traversable)

runReader :: Reader r a -> r -> a
runReader (Reader f) = f

positive :: Int -> Maybe Int
positive x = if x > 0 then Just x else Nothing

main :: IO ()
main = do
  let t = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf) :: Tree Int
      n = Nested [1, 2] (Just 3) (0, 4) [('a', [5, 6]), ('b', [7])] :: Nested Int
  print (fmap (* 2) t)
  print (toList t, sum t, length t, maximum t, elem 3 t, foldr (:) [] t)
  print (traverse positive t)
  print (fmap toList (traverse (\x -> if x == 2 then Nothing else Just x) t))
  print (fmap show (Pair 1 (2 :: Int)), sum (Pair 3 (4 :: Int)), sequenceA (Pair [1, 2] [3 :: Int]))
  print (fmap not (Tagged 'k' True), fmap not (Untagged False :: Tagged Char Bool), toList (Tagged 'k' 'v'))
  print (fmap (+ 1) n)
  print (toList n, product n)
  print (traverse positive n)
  let r = Rose 1 [Rose 2 [Rose 4 []], Rose 3 []] :: Rose Int
  print (fmap (* 10) r, toList r, sum r)
  mapM_ print (traverse (\x -> [x, x + 100]) (Rose 1 [Rose (2 :: Int) []]))
  print (runReader (fmap (+ 1) (Reader (* 2))) (5 :: Int))
  print (toList (Wrap (Just 'w')), toList (Both "ab" ["cd", "e"]), sum (fmap length (Wrap ["one", "three"])))
  print (case traverse positive (Both [1, 2] [[3]]) of Just w -> toList w; Nothing -> [])
  print (fmap succ (Record 'a' 2 "bc"), toList (Record 'a' 2 "bc"), count (fmap succ (Record 'a' 2 "bc")))
  print (fmap negate (L 'l' :: Sum' Char Int), fmap negate (R 1 :: Sum' Char Int), fmap negate (2 :*: 3 :: Sum' Char Int), sum (4 :*: 5 :: Sum' Char Int))
  print (fmap not (Phantom 7), toList (Phantom 7 :: Phantom Bool), traverse positive (Phantom 7))
  r' <- traverse (\x -> print x >> return (x * 2)) (Pair 1 (2 :: Int))
  print r'
