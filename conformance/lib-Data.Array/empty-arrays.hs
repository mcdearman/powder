-- Haskell 2010 Report 14 (Data.Array): empty arrays (lower bound greater than upper bound)
module Main (main) where

import Data.Array

e1 :: Array Int Char
e1 = array (1, 0) []

e2 :: Array Int Char
e2 = listArray (5, 2) "ignored"

e2d :: Array (Int, Int) Int
e2d = listArray ((0, 0), (2, -1)) []

main :: IO ()
main = do
  print ("bounds", bounds e1)
  print ("indices", indices e1)
  print ("elems", elems e1)
  print ("assocs", assocs e1)
  print ("show", e1)
  print ("listArray empty bounds", bounds e2)
  print ("listArray empty elems", elems e2)
  print ("empty equals itself", e1 == e1)
  print ("compare empty", compare e1 e1)
  print ("update with nothing", elems (e1 // []))
  print ("fmap", elems (fmap succ e1))
  print ("accumArray empty", elems (accumArray (+) 0 (3, 2) [] :: Array Int Int))
  print ("accum empty", elems (accum (+) (listArray (1, 0) [] :: Array Int Int) []))
  print ("ixmap to empty", elems (ixmap (1, 0) id (listArray (0, 3) "abcd" :: Array Int Char)))
  print ("2d empty bounds", bounds e2d)
  print ("2d empty elems", elems e2d)
  print ("2d empty indices", indices e2d)
  print ("empty char index", elems (listArray ('z', 'a') [] :: Array Char Int))
  print ("length", length (elems e1))
  print ("read empty", bounds (read "array (1,0) []" :: Array Int Int))
