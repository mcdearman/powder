-- Haskell 2010 Report 5.1: the program is the module Main exporting main; Main may export other entities and name itself qualified
module Main (main, helper, Main.other) where

import Lib (twice)

helper :: Int -> Int
helper = twice . (+ 1)

other :: Int
other = 7

main :: IO ()
main = do
  print ("helper", helper 1)
  print ("qualified self reference", Main.helper 2, Main.other)
  print ("qualified main exists", const True Main.main)
