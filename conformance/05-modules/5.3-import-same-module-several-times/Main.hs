-- Haskell 2010 Report 5.3: several import declarations for the same module accumulate; an entity imported twice is not a clash
module Main (main) where

import Zoo (Animal(..))
import Zoo (legs)
import Zoo (legs, feed)
import Zoo hiding (legs, zooName, Cage(..))
import qualified Zoo as Q (zooName)
import Data.List (sort)
import Data.List (nub, sort)

zooName :: String
zooName = "Main's zoo"

main :: IO ()
main = do
  print ("constructors from first import", [Cat, Dog, Emu])
  print ("imported twice, no ambiguity", legs Cat, feed Dog)
  print ("from the hiding import", noise Emu, 1 <+> 2)
  print ("hidden in one import but imported by another", legs Emu)
  print ("local vs qualified-only", zooName, Q.zooName)
  print ("library imported twice", sort (nub [3, 1, 3, 2 :: Int]))
