-- Haskell 2010 Report 19.1 (Data.Ix): the Ix laws spot-checked
--   inRange (l,u) i == elem i (range (l,u))
--   range (l,u) !! index (l,u) i == i, when inRange (l,u) i
--   map (index (l,u)) (range (l,u)) == [0..rangeSize (l,u)-1]
--   rangeSize (l,u) == length (range (l,u))
module Main (main) where

import Data.Ix

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Show, Ix)

law1 :: (Ix a) => (a, a) -> [a] -> Bool
law1 b candidates = all (\i -> inRange b i == elem i (range b)) candidates

law2 :: (Ix a) => (a, a) -> Bool
law2 b = all (\i -> range b !! index b i == i) (filter (inRange b) (range b))

law3 :: (Ix a) => (a, a) -> Bool
law3 b = map (index b) (range b) == [0 .. rangeSize b - 1]

law4 :: (Ix a) => (a, a) -> Bool
law4 b = rangeSize b == length (range b)

check :: (Ix a) => String -> (a, a) -> [a] -> IO ()
check name b candidates = print (name, law1 b candidates, law2 b, law3 b, law4 b)

main :: IO ()
main = do
  check "Int" (-3, 4 :: Int) [-6 .. 6]
  check "Int empty" (4, -3 :: Int) [-6 .. 6]
  check "Int singleton" (0, 0 :: Int) [-1, 0, 1]
  check "Integer" (10 ^ 12, 10 ^ 12 + 9 :: Integer) [10 ^ 12 - 2 .. 10 ^ 12 + 11]
  check "Char" ('c', 'k') ['a' .. 'm']
  check "Bool" (False, True) [False, True]
  check "Bool upper half" (True, True) [False, True]
  check "Ordering" (LT, EQ) [LT, EQ, GT]
  check "Suit" (Diamonds, Spades) [Clubs, Diamonds, Hearts, Spades]
  check "pair" ((0, 0), (2, 3) :: (Int, Int)) [(i, j) | i <- [-1 .. 3], j <- [-1 .. 4]]
  check "pair empty" ((0, 3), (2, 0) :: (Int, Int)) [(i, j) | i <- [-1 .. 3], j <- [-1 .. 4]]
  check "pair mixed" ((False, 'a'), (True, 'c')) [(p, c) | p <- [False, True], c <- ['a' .. 'd']]
  check "triple" ((0, 0, 0), (1, 2, 3) :: (Int, Int, Int)) [(i, j, k) | i <- [0 .. 2], j <- [0 .. 3], k <- [0 .. 4]]
  check "4-tuple" ((0, 'a', False, LT), (1, 'b', True, GT) :: (Int, Char, Bool, Ordering)) [(i, c, p, o) | i <- [0, 1, 2], c <- "abc", p <- [False, True], o <- [LT, GT]]
  check "pair of suits" ((Clubs, Hearts), (Hearts, Spades)) [(s, t) | s <- range (Clubs, Spades), t <- range (Clubs, Spades)]
  print ("range of a tuple is the product of the ranges", range ((1, 'a'), (2, 'b')) == [(i, c) | i <- range (1, 2 :: Int), c <- range ('a', 'b')])
  print ("rangeSize of a tuple is the product of the sizes", rangeSize ((1, 'a'), (5, 'f') :: (Int, Char)) == rangeSize (1, 5 :: Int) * rangeSize ('a', 'f'))
  print ("index is strictly increasing along range", and (zipWith (<) (map (index ((0, 0), (3, 3) :: (Int, Int))) (range ((0, 0), (3, 3)))) [1 ..]))
