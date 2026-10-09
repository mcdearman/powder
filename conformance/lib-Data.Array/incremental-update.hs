-- Haskell 2010 Report 14.2 (Data.Array): incremental update with (//), and its fixity with (!)
module Main (main) where

import Data.Array

a0 :: Array Int Char
a0 = listArray (0, 4) "abcde"

swapElems :: Int -> Int -> Array Int Char -> Array Int Char
swapElems i j a = a // [(i, a ! j), (j, a ! i)]

main :: IO ()
main = do
  print ("update one", elems (a0 // [(2, 'X')]))
  print ("update several", elems (a0 // [(4, 'Z'), (0, 'A')]))
  print ("update none", (a0 // []) == a0)
  print ("original unchanged", elems a0)
  print ("bounds preserved", bounds (a0 // [(1, 'q')]))
  print ("chained updates", elems (a0 // [(0, '1')] // [(1, '2')] // [(0, '3')]))
  print ("swap", elems (swapElems 0 4 a0))
  print ("update all", elems (a0 // [(i, 'x') | i <- indices a0]))
  print ("index binds tighter than update", elems (a0 // [(0, a0 ! 4)]))
  print ("index is infixl 9", a0 ! 1 : [a0 ! 2])
  print ("update then index", (a0 // [(3, 'D')]) ! 3)
  print ("update equals rebuilt array", (a0 // [(1, 'B')]) == listArray (0, 4) "aBcde")
  print ("update differs from original", (a0 // [(1, 'B')]) /= a0)
