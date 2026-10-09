-- Haskell 2010 Report 4.2.1: a field label shared by several constructors of one datatype
module Main (main) where

data Shape = Circle { label :: String, radius :: Int }
           | Rect { label :: String, width :: Int, height :: Int }
           | Dot { label :: String }
  deriving (Eq, Show)

area :: Shape -> Int
area Circle { radius = r } = 3 * r * r
area Rect { width = w, height = h } = w * h
area Dot {} = 0

rename :: String -> Shape -> Shape
rename s shape = shape { label = s }

shapes :: [Shape]
shapes = [Circle "c" 2, Rect { label = "r", width = 3, height = 4 }, Dot "d"]

main :: IO ()
main = do
  print ("shared selector", map label shapes)
  print ("areas", map area shapes)
  print ("show circle", Circle { radius = 1, label = "one" })
  print ("show rect", Rect "two" 2 3)
  print ("show dot", Dot { label = "three" })
  print ("update shared on circle", rename "x" (Circle "c" 2))
  print ("update shared on rect", rename "y" (Rect "r" 3 4))
  print ("update shared on dot", rename "z" (Dot "d"))
  print ("update shared on all", map (label . rename "same") shapes)
  print ("update two fields", (Rect "r" 3 4) { width = 10, height = 20 })
  print ("update own field", (Circle "c" 2) { radius = 5 })
  print ("update shared and own", (Circle "c" 2) { label = "big", radius = 50 })
  print ("equality", rename "c" (Circle "q" 2) == Circle "c" 2)
