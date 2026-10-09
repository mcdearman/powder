-- Haskell 2010 Report 14.3 (Data.Array): derived arrays with ixmap, and the Functor instance (fmap)
module Main (main) where

import Data.Array

v :: Array Int Int
v = listArray (0, 4) [10, 20, 30, 40, 50]

m :: Array (Int, Int) Char
m = listArray ((0, 0), (1, 2)) "abcdef"

row :: Int -> Array (Int, Int) Char -> Array Int Char
row i a = ixmap (c0, c1) (\j -> (i, j)) a
  where
    ((_, c0), (_, c1)) = bounds a

transposed :: Array (Int, Int) Char -> Array (Int, Int) Char
transposed a = ixmap ((c0, r0), (c1, r1)) (\(j, i) -> (i, j)) a
  where
    ((r0, c0), (r1, c1)) = bounds a

main :: IO ()
main = do
  print ("reverse", elems (ixmap (0, 4) (\i -> 4 - i) v))
  print ("slice", assocs (ixmap (1, 3) id v))
  print ("shift indices", assocs (ixmap (10, 12) (\i -> i - 10) v))
  print ("duplicate elements", elems (ixmap (0, 5) (`div` 2) v))
  print ("identity", ixmap (bounds v) id v == v)
  print ("row 0", elems (row 0 m))
  print ("row 1", elems (row 1 m))
  print ("column 2", elems (ixmap (0, 1) (\i -> (i, 2)) m))
  print ("transpose bounds", bounds (transposed m))
  print ("transpose elems", elems (transposed m))
  print ("diagonal to char index", assocs (ixmap ('a', 'b') (\c -> (fromEnum c - 97, fromEnum c - 97)) m))
  print ("fmap", elems (fmap (`div` 10) v))
  print ("fmap keeps bounds", bounds (fmap show v))
  print ("fmap changes type", elems (fmap show v))
  print ("fmap 2d", elems (fmap succ m))
  print ("fmap identity", fmap id v == v)
  print ("fmap composition", fmap (negate . (+ 1)) v == fmap negate (fmap (+ 1) v))
