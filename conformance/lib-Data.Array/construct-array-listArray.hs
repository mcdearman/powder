-- Haskell 2010 Report 14.1 (Data.Array): array, listArray, bounds, (!), indices, elems, assocs
module Main (main) where

import Data.Array

squares :: Array Int Int
squares = array (1, 5) [(i, i * i) | i <- [1 .. 5]]

shuffled :: Array Int Char
shuffled = array (0, 3) [(2, 'c'), (0, 'a'), (3, 'd'), (1, 'b')]

letters :: Array Int Char
letters = listArray (10, 14) "hello"

negs :: Array Int Int
negs = listArray (-2, 2) [20, 10, 0, 11, 22]

main :: IO ()
main = do
  print ("bounds", bounds squares)
  print ("index 1", squares ! 1)
  print ("index 5", squares ! 5)
  print ("indices", indices squares)
  print ("elems", elems squares)
  print ("assocs", assocs squares)
  print ("unordered association list", elems shuffled)
  print ("assocs are in index order", assocs shuffled)
  print ("listArray bounds", bounds letters)
  print ("listArray index", letters ! 10, letters ! 14)
  print ("listArray elems", elems letters)
  print ("negative lower bound", negs ! (-2), negs ! 0, negs ! 2)
  print ("negative indices", indices negs)
  print ("listArray ignores extra elements", elems (listArray (1, 3) [1 ..] :: Array Int Int))
  print ("singleton", assocs (array (7, 7) [(7, "only")] :: Array Int String))
  print ("array from assocs roundtrip", array (bounds squares) (assocs squares) == squares)
  print ("listArray from elems roundtrip", listArray (bounds letters) (elems letters) == letters)
  print ("elems via index", [squares ! i | i <- indices squares] == elems squares)
  print ("length elems", length (elems negs))
