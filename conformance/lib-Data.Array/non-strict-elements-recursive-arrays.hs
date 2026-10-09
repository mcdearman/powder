-- Haskell 2010 Report 14.1 (Data.Array): arrays are non-strict in their elements; recursively defined arrays
module Main (main) where

import Data.Array

holes :: Array Int Int
holes = listArray (0, 3) [1, undefined, 3, undefined]

partial :: Array Int Char
partial = array (0, 2) [(0, 'a'), (2, 'c')]

fibs :: Int -> Array Int Integer
fibs n = a
  where
    a = array (0, n) ([(0, 0), (1, 1)] ++ [(i, a ! (i - 1) + a ! (i - 2)) | i <- [2 .. n]])

-- Pascal's triangle by lazy dynamic programming over a 2-D array
binomials :: Int -> Array (Int, Int) Integer
binomials n = t
  where
    t = array ((0, 0), (n, n)) [((i, j), cell i j) | i <- [0 .. n], j <- [0 .. n]]
    cell i j
      | j == 0 = 1
      | j > i = 0
      | otherwise = t ! (i - 1, j - 1) + t ! (i - 1, j)

-- longest common subsequence length, table defined in terms of itself
lcs :: String -> String -> Int
lcs xs ys = table ! (0, 0)
  where
    n = length xs
    m = length ys
    xa = listArray (0, n - 1) xs :: Array Int Char
    ya = listArray (0, m - 1) ys :: Array Int Char
    table = array ((0, 0), (n, m)) [((i, j), f i j) | i <- [0 .. n], j <- [0 .. m]] :: Array (Int, Int) Int
    f i j
      | i == n || j == m = 0
      | xa ! i == ya ! j = 1 + table ! (i + 1, j + 1)
      | otherwise = max (table ! (i + 1, j)) (table ! (i, j + 1))

main :: IO ()
main = do
  print ("defined elements", holes ! 0, holes ! 2)
  print ("bounds of array with undefined elements", bounds holes)
  print ("indices", indices holes)
  print ("length elems", length (elems holes))
  print ("missing association never forced", partial ! 0, partial ! 2)
  print ("fmap is lazy", fmap (+ 1) holes ! 2)
  print ("update over undefined", elems (holes // [(1, 2), (3, 4)]))
  print ("fib 10", fibs 10 ! 10)
  print ("fib 90", fibs 90 ! 90)
  print ("fib table", elems (fibs 12))
  print ("binomial row 6", [binomials 6 ! (6, j) | j <- [0 .. 6]])
  print ("binomial 30 15", binomials 30 ! (30, 15))
  print ("lcs", lcs "AGGTAB" "GXTXAYB")
  print ("lcs empty", lcs "" "abc")
  print ("lcs equal", lcs "haskell" "haskell")
