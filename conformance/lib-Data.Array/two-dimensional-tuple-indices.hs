-- Haskell 2010 Report 14 (Data.Array): multi-dimensional arrays indexed by tuples, in row-major order
module Main (main) where

import Data.Array

type Matrix = Array (Int, Int) Integer

fromRows :: [[Integer]] -> Matrix
fromRows rows = listArray ((1, 1), (length rows, length (head rows))) (concat rows)

mmul :: Matrix -> Matrix -> Matrix
mmul x y =
  array
    ((r0, c0), (r1, c1))
    [((i, j), sum [x ! (i, k) * y ! (k, j) | k <- range (k0, k1)]) | i <- range (r0, r1), j <- range (c0, c1)]
  where
    ((r0, k0), (r1, k1)) = bounds x
    ((_, c0), (_, c1)) = bounds y

a :: Matrix
a = fromRows [[1, 2, 3], [4, 5, 6]]

b :: Matrix
b = fromRows [[7, 8], [9, 10], [11, 12]]

cube :: Array (Int, Int, Int) Int
cube = listArray ((0, 0, 0), (1, 1, 1)) [0 ..]

main :: IO ()
main = do
  print ("bounds", bounds a)
  print ("indices row major", indices a)
  print ("elems", elems a)
  print ("index", a ! (1, 1), a ! (1, 3), a ! (2, 1), a ! (2, 3))
  print ("assocs", take 4 (assocs a))
  print ("product bounds", bounds (mmul a b))
  print ("product", elems (mmul a b))
  print ("product other way", elems (mmul b a))
  print ("update", elems (a // [((2, 2), 0)]))
  print ("accumArray 2d", elems (accumArray (+) 0 ((0, 0), (1, 1)) [((i `mod` 2, i `div` 2 `mod` 2), i) | i <- [0 .. 7]] :: Array (Int, Int) Int))
  print ("3d indices", indices cube)
  print ("3d elems", elems cube)
  print ("3d index", cube ! (1, 0, 1))
  print ("show 2d", a)
  print ("mixed index types", assocs (listArray ((False, 'a'), (True, 'b')) [1 ..] :: Array (Bool, Char) Int))
