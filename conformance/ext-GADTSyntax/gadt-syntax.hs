{-# LANGUAGE GADTSyntax #-}
-- GADTSyntax: a type's constructors given by their types, after `where`
module Main (main) where

data Maybe' a where
  Nothing' :: Maybe' a
  Just' :: a -> Maybe' a
  deriving (Eq, Show)

data Tree a where
  Leaf :: Tree a
  Node :: Tree b -> b -> Tree b -> Tree b

-- The type's parameters may be named as each constructor likes.
data Either' a b where
  L :: x -> Either' x y
  R :: y -> Either' x y
  deriving (Show)

data Person where
  Person :: {name :: String, age :: !Int} -> Person
  deriving (Show, Eq)

data Strict where
  Strict :: !Int -> Int -> Strict

data List a where { Nil :: List a; (:::) :: a -> List a -> List a }

infixr 5 :::

newtype Age where
  Age :: Int -> Age
  deriving (Show)

data Fn a b where
  Fn :: (a -> b) -> [a] -> Fn a b

toList :: Tree a -> [a]
toList Leaf = []
toList (Node l x r) = toList l ++ [x] ++ toList r

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x t@(Node l y r)
  | x < y = Node (insert x l) y r
  | x > y = Node l y (insert x r)
  | otherwise = t

len :: List a -> Int
len Nil = 0
len (_ ::: rest) = 1 + len rest

main :: IO ()
main = do
  print (Just' 3, Nothing' :: Maybe' Int, Just' 'a' == Just' 'a')
  print (toList (foldr insert Leaf [3, 1, 2 :: Int]))
  print [L 1 :: Either' Int Char, R 'c']
  let p = Person {name = "Ann", age = 30}
  print (p, name p, age p, p {age = 31} == p)
  print (case Strict 1 undefined of Strict a _ -> a)
  print (len (1 ::: 2 ::: 3 ::: Nil))
  print (Age 4)
  print (case Fn show [1, 2 :: Int] of Fn f xs -> map f xs)
