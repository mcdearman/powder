-- Modules: imports that are qualified, renamed, listed and hiding; an
-- export list; a module exported again; and two modules, Even and Odd,
-- that import each other.
module Main (main) where

import Parts.Odd
import qualified Parts.Shapes
import qualified Parts.Shapes as S (Shape (..), area)
import Prelude hiding (lookup)
import Parts.Wrap

-- Main's own, in place of the one the prelude would have given.
lookup :: Int -> String
lookup n = "entry " ++ show n

length' :: [a] -> Int
length' = length

main :: IO ()
main = do
  print (isOdd 7, isEven 7, describe 10)
  print (describe 3 == Odd)
  print (S.area (S.Circle 2), Parts.Shapes.area (Parts.Shapes.Square 3), Parts.Shapes.length (S.Square 3))
  putStrLn (lookup 3)
  print (length' "four", Prelude.length [1, 2, 3])
  -- A newtype's constructor is not looked at, an imported operator keeps
  -- the fixity its module gave it, and an imported instance is there.
  print (case undefined of Age _ -> "not looked at", 1 : [2] <+> [3] <+> [], Age 3 + 4, older 5 (Age 2))
  print (case 2.5 of { 2.5 -> "half"; _ -> "other" }, [d | (d, 0.5) <- [('a', 0.5), ('b', 1.5)]])
