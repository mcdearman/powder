-- Haskell 2010 Report 5.3.1: "It is not an error to hide an entity that is not, in fact, exported by the imported module"
module Main (main) where

import Lib hiding (notExportedByLib, AlsoNotThere, present2)
import Data.List hiding (thisIsNotInDataList)

present2 :: Int
present2 = 22

main :: IO ()
main = do
  print ("remaining import", present1)
  print ("hidden and redefined", present2)
  print ("library import still works", sort [2, 1, 3 :: Int])
