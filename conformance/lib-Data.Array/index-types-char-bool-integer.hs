-- Haskell 2010 Report 14 (Data.Array): arrays indexed by Char, Bool, Integer, Ordering and a derived Ix type;
-- Data.Array re-exports Data.Ix
module Main (main) where

import Data.Array

data Day = Mon | Tue | Wed | Thu | Fri
  deriving (Eq, Ord, Show, Ix)

counts :: String -> Array Char Int
counts s = accumArray (+) 0 ('a', 'e') [(c, 1) | c <- s, inRange ('a', 'e') c]

truth :: Array Bool String
truth = listArray (False, True) ["no", "yes"]

big :: Array Integer Char
big = listArray (10 ^ 20, 10 ^ 20 + 2) "xyz"

ords :: Array Ordering Int
ords = array (LT, GT) [(GT, 1), (EQ, 0), (LT, -1)]

hours :: Array Day Int
hours = listArray (Mon, Fri) [8, 8, 6, 8, 4]

main :: IO ()
main = do
  print ("char counts", assocs (counts "abracadabra deed"))
  print ("char index", counts "cab" ! 'c')
  print ("char bounds", bounds (counts ""))
  print ("bool", truth ! True, truth ! False)
  print ("bool indices", indices truth)
  print ("integer bounds", bounds big)
  print ("integer index", big ! (10 ^ 20 + 1))
  print ("integer indices", indices big)
  print ("ordering", [ords ! compare x 5 | x <- [3, 5, 7 :: Int]])
  print ("ordering assocs", assocs ords)
  print ("derived ix", hours ! Wed)
  print ("derived ix assocs", assocs hours)
  print ("derived ix sum", sum (elems hours))
  print ("derived ix slice", elems (ixmap (Tue, Thu) id hours))
  print ("show derived ix", hours // [(Fri, 0)])
  print ("range re-exported", range ('a', 'd'))
  print ("index re-exported", index (Mon, Fri) Thu)
  print ("rangeSize re-exported", rangeSize (bounds hours))
