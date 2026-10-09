-- Haskell 2010 Report 11.4: "readsPrec will parse any valid representation produced by showsPrec":
-- (x,"") is an element of (readsPrec d (showsPrec d x "")) for every d from 0 to 11
module Main (main) where

import Data.Ratio ((%))

infixr 5 :+:

infixl 6 :-:

infix 4 :=:

infixr 5 `Cat`

data Color = Red | Green | Blue deriving (Eq, Show, Read)

data P = P Int Double deriving (Eq, Show, Read)

data E = L Int | E :+: E | E :-: E | E :=: E | E :*: E | Neg E deriving (Eq, Show, Read)

data Rec = Rec {name :: String, val :: Int, sub :: Maybe Rec} deriving (Eq, Show, Read)

data O = (:@:) Int Char deriving (Eq, Show, Read)

data S = String `Cat` String deriving (Eq, Show, Read)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show, Read)

data Mix = Mix () (Int, Char) String [Int] Char (Maybe Bool) (Either Int Ordering) deriving (Eq, Show, Read)

data Q = Q Rational Integer Float deriving (Eq, Show, Read)

newtype W a = W {unW :: a} deriving (Eq, Show, Read)

-- round trip through showsPrec d / readsPrec d, keeping only complete parses
rt :: (Eq a, Show a, Read a) => Int -> a -> Bool
rt d x = [y | (y, "") <- readsPrec d (showsPrec d x "")] == [x]

allPrecs :: (Eq a, Show a, Read a) => a -> Bool
allPrecs x = all (\d -> rt d x) [0 .. 11]

check :: (Eq a, Show a, Read a) => String -> [a] -> IO ()
check label xs = print (label, all allPrecs xs, all (\x -> read (show x) == x) xs, length xs)

main :: IO ()
main = do
  check "enumeration" [Red, Green, Blue]
  check "prefix with negative numbers" [P 1 2.5, P (-1) 2.5, P 1 (-2.5), P minBound (-1.0e-3), P maxBound 1.0e100, P 0 0.1]
  check "infix" [L 1 :+: L 2, L 1 :+: (L 2 :+: L 3), (L 1 :+: L 2) :+: L 3, L 1 :-: L 2 :+: L 3, L 1 :+: L 2 :-: L 3]
  check "infix levels" [L 1 :=: L 2, (L 1 :=: L 2) :*: L 3, L 1 :*: L 2 :*: L 3, L 1 :*: L 2 :-: L 3 :*: L 4 :+: L 5 :=: L 6, L (-1) :*: L (-2)]
  check "infix with prefix" [Neg (L 1), Neg (L 1 :+: L 2), Neg (L 1) :+: Neg (L (-2)), Neg (Neg (L 1 :*: Neg (L 2)))]
  check "records" [Rec "a" 1 Nothing, Rec "" (-1) (Just (Rec "b\"c" minBound Nothing)), Rec "x" 0 (Just (Rec "y" 1 (Just (Rec "z" 2 Nothing))))]
  check "operator constructor declared prefix" [(:@:) 1 'a', (:@:) (-1) '\'', (:@:) 0 '\n']
  check "backquoted constructor" ["a" `Cat` "b", "" `Cat` "", "with `backquote`" `Cat` "and \"quotes\""]
  check "tree of Int" [Leaf, Node Leaf (1 :: Int) Leaf, Node (Node Leaf (-1) Leaf) 0 (Node Leaf 1 Leaf)]
  check "tree of Maybe" [Node Leaf (Just (-1 :: Int)) (Node Leaf Nothing Leaf)]
  check "tree of derived" [Node Leaf (P (-1) 0.5) Leaf, Node (Node Leaf (P 0 0) Leaf) (P 1 1) Leaf]
  check "mixed field types" [Mix () (1, 'a') "s" [1, 2] 'c' (Just True) (Left (-1)), Mix () (-1, '"') "" [] '\\' Nothing (Right GT)]
  check "rational integer float" [Q (1 % 2) 3 0.5, Q (-1 % 2) (-3) (-0.5), Q 0 (2 ^ (70 :: Int)) 1.0e-3]
  check "newtype record" [W 'x', W '\'']
  check "newtype of derived" [W (P (-1) 1), W (P 2 (-2))]
  check "nested newtype" [W (W (-1 :: Int)), W (W 0)]
  check "lists of derived" [[Red, Blue], [], [Green]]
  check "tuples of derived" [(Red, P (-1) 0, L 1 :+: L 2), (Blue, P 0 (-1), Neg (L (-1)))]
  check "Maybe of derived" [Just (P (-1) (-1)), Nothing]
  check "Maybe of infix" [Just (L 1 :=: L 2), Just (Neg (L 0))]
  check "Either of derived" [Left Red, Right (Rec "r" (-1) Nothing)]
  check "Prelude types" [Just (-1 :: Int), Nothing]
  check "Prelude nested" [[Left (-1.5 :: Double), Right (Just [LT, GT])]]
