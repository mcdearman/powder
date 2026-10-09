-- Haskell 2010 Report 5.2: export entity forms: var, T, T(..), T(C1,C2), T(field), class C, C(..), C(m), type synonyms
module Main (main) where

import Shapes

-- names that Shapes does NOT export can be defined here without any clash
hidden :: Int
hidden = 0

data Secret = Rhombus deriving Show

sides :: Int
sides = 99

main :: IO ()
main = do
  print ("all constructors T(..)", [Circle 1, Square 2])
  print ("some constructors T(A,B)", [North, South], isHiddenDir (opposite North))
  print ("abstract T", describeOpaque (mkOpaque 5))
  print ("field subset", px (mkPoint 1 2), py (mkPoint 1 2))
  print ("field update through exported field", px ((mkPoint 1 2) { px = 10 }))
  print ("synonym", (origin :: Coord))
  print ("class C(..)", area (Circle 2), perimeter (Square 3))
  print ("class C(m) subset", name (Circle 1))
  print ("class C abstract, used via function", sizeOf (Square 4))
  print ("unexported names free for reuse", hidden, Rhombus, sides)
