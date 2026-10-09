-- Haskell 2010 Report 14 (Data.Array): Eq and Ord instances of Array (comparison of association lists)
module Main (main) where

import Data.Array

mk :: (Int, Int) -> [Int] -> Array Int Int
mk = listArray

main :: IO ()
main = do
  print ("equal", mk (0, 2) [1, 2, 3] == mk (0, 2) [1, 2, 3])
  print ("different element", mk (0, 2) [1, 2, 3] == mk (0, 2) [1, 2, 4])
  print ("different bounds same elems", mk (0, 2) [1, 2, 3] == mk (1, 3) [1, 2, 3])
  print ("not equal", mk (0, 2) [1, 2, 3] /= mk (0, 2) [1, 9, 3])
  print ("built differently", mk (0, 2) [1, 2, 3] == array (0, 2) [(2, 3), (1, 2), (0, 1)])
  print ("compare equal", compare (mk (0, 1) [1, 2]) (mk (0, 1) [1, 2]))
  print ("compare by element", compare (mk (0, 1) [1, 2]) (mk (0, 1) [1, 3]))
  print ("compare first element decides", compare (mk (0, 1) [2, 0]) (mk (0, 1) [1, 9]))
  print ("compare index before element", compare (mk (0, 1) [9, 9]) (mk (1, 2) [0, 0]))
  print ("compare prefix", compare (mk (0, 1) [1, 2]) (mk (0, 2) [1, 2, 3]))
  print ("less than", mk (0, 1) [1, 2] < mk (0, 1) [1, 3])
  print ("less or equal", mk (0, 1) [1, 2] <= mk (0, 1) [1, 2])
  print ("greater", mk (0, 1) [5, 0] > mk (0, 1) [4, 9])
  print ("max", elems (max (mk (0, 1) [1, 2]) (mk (0, 1) [1, 3])))
  print ("min", elems (min (mk (0, 1) [1, 2]) (mk (0, 1) [1, 3])))
  print ("maximum of arrays", elems (maximum [mk (0, 0) [3], mk (0, 0) [7], mk (0, 0) [5]]))
  print ("nested arrays equal", listArray (0, 1) [mk (0, 0) [1], mk (0, 0) [2]] == (listArray (0, 1) [mk (0, 0) [1], mk (0, 0) [2]] :: Array Int (Array Int Int)))
