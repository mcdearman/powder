{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}
-- TypeFamilies: functions from types to a type, with equations beside them, apart from them, or in the instances of
-- the class they go with
module Main (main) where

import Data.Kind (Type)
import qualified Data.List as L

-- With all its equations, tried in order.
type family Elem c where
  Elem [a] = a
  Elem (Maybe a) = a
  Elem (a, b) = a

type family Unwrap a where
  Unwrap (Maybe a) = Unwrap a
  Unwrap [a] = Unwrap a
  Unwrap a = a

-- Open to equations given apart from it.
type family Key m :: Type

type instance Key [(k, v)] = k

type instance Key (Assoc k v) = k

newtype Assoc k v = Assoc [(k, v)]

-- A type that goes with a class, given in each instance.
class Container f where
  type Item f
  empty :: f
  insert :: Item f -> f -> f
  toL :: f -> [Item f]

instance Container [a] where
  type Item [a] = a
  empty = []
  insert = (:)
  toL = id

newtype IntSet = IntSet [Int]

instance Container IntSet where
  type Item IntSet = Int
  empty = IntSet []
  insert x (IntSet xs) = IntSet (L.insert x (filter (/= x) xs))
  toL (IntSet xs) = xs

newtype Text' = Text' String

instance Container Text' where
  type Item Text' = Char
  empty = Text' ""
  insert c (Text' s) = Text' (c : s)
  toL (Text' s) = s

class Collapse a where
  type Result a :: Type
  collapse :: a -> Result a

instance Collapse Bool where
  type Result Bool = Int
  collapse b = if b then 1 else 0

instance Collapse a => Collapse [a] where
  type Result [a] = [Result a]
  collapse = map collapse

instance Collapse Char where
  type Result Char = String
  collapse c = [c, c]

firstOf :: [a] -> Elem [a]
firstOf = head

fromJust' :: Maybe a -> Elem (Maybe a)
fromJust' (Just x) = x
fromJust' Nothing = error "nothing"

left :: (a, b) -> Elem (a, b)
left = fst

deep :: [Maybe [Int]] -> [Unwrap [Maybe [Int]]]
deep xss = [x | Just xs <- xss, x <- xs]

keys :: [(k, v)] -> [Key [(k, v)]]
keys = map fst

assocKeys :: Assoc k v -> [Key (Assoc k v)]
assocKeys (Assoc kvs) = map fst kvs

fill :: Container f => [Item f] -> f
fill = foldr insert empty

total :: (Container f, Num (Item f)) => f -> Item f
total = sum . toL

main :: IO ()
main = do
  print (firstOf "abc", fromJust' (Just True), left (1 :: Int, 'x'))
  print (deep [Just [1, 2], Nothing, Just [3]])
  print (keys [(1 :: Int, 'a'), (2, 'b')], assocKeys (Assoc [("k", ())]))
  print (toL (fill [3, 1, 2, 1] :: IntSet), toL (fill "abc" :: Text'), toL (fill [True, False] :: [Bool]))
  print (total (fill [1, 2, 3] :: IntSet), total [1.5, 2.5 :: Double])
  print (collapse True, collapse [True, False], collapse "ab", collapse [[True], []])
  let x = (firstOf [10, 20 :: Int] :: Int) + 1
  print x
