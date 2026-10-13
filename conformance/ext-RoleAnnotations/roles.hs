{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RoleAnnotations, TypeFamilies #-}
-- RoleAnnotations: what a type's parameter is to the type, which says when the type at one type may be taken as
-- the type at another. Found from the type's fields where it is not said
module Main (main) where

import Data.Coerce (coerce)

newtype Age = Age Int deriving (Show)

newtype Name = Name String deriving (Show)

-- A parameter the fields have values of: kept as it is.
data Pair a b = Pair a b deriving (Show)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

-- One the fields have nothing of: anything.
data Tagged tag a = Tagged a deriving (Show)

-- Said to be kept as it is, where nothing of it is had.
type role Labelled representational representational

data Labelled label a = Labelled a deriving (Show)

-- Said to be the same type, and at the same type it is.
type role Sorted nominal

data Sorted a = Sorted [a] deriving (Show)

-- Left to be found, by an underscore.
type role Both _ representational

data Both a b = Both a b deriving (Show)

-- Through types that are applied, and a function.
data Nested a = Nested (Maybe a) [Maybe a] (a -> Int)

ages :: [Int] -> [Age]
ages = coerce

main :: IO ()
main = do
  print (coerce (Pair (Age 1) (Name "a")) :: Pair Int String)
  print (coerce (Node Leaf (Age 2) (Node Leaf (Age 3) Leaf)) :: Tree Int)
  print (coerce (Tagged (Age 4) :: Tagged Bool Age) :: Tagged Char Int)
  print (coerce (Labelled 'x' :: Labelled Age Char) :: Labelled Int Char)
  print (coerce (Sorted [Age 5]) :: Sorted Age)
  print (coerce (Both (Age 6) (Age 7)) :: Both Int Int)
  print (ages [8, 9], coerce (Just [Age 10]) :: Maybe [Int])
  case coerce (Nested (Just (Age 11)) [Just (Age 12)] (\(Age n) -> n)) :: Nested Int of
    Nested a bs f -> print (a, bs, f 13)
