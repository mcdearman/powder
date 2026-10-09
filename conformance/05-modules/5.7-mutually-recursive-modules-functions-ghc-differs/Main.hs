-- Haskell 2010 Report 5 (intro) / 5.7: modules may be mutually recursive: Even imports Odd and Odd imports Even.
-- GHC only supports this with .hs-boot files, so the expected output (expected-output.txt) was derived by hand.
module Main (main) where

import Even (isEven, evens)
import Odd (isOdd, odds)

main :: IO ()
main = do
  print ("isEven", map isEven [0, 1, 2, 7, 10])
  print ("isOdd", map isOdd [0, 1, 2, 7, 10])
  print ("evens", take 5 evens)
  print ("odds", take 5 odds)
