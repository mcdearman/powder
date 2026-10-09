-- Haskell 2010 Report 5.1: module names are dotted sequences of conids; modules with the same last component are distinct
module Main (main) where

import Geo.Shapes.Circle
import qualified Geo.Shapes.Square
import qualified Geo.Util
import qualified Text.Util

main :: IO ()
main = do
  print ("circle area", area 2)
  print ("square area", Geo.Shapes.Square.area 2)
  print ("geo util", Geo.Util.describe 3)
  print ("text util", Text.Util.describe 3)
  print ("deep", Geo.Shapes.Square.viaUtil 4)
