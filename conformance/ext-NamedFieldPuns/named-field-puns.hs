{-# LANGUAGE NamedFieldPuns #-}
-- NamedFieldPuns: a field by itself in a pattern binds the variable of its name, and in an expression is it
module Main (main) where

data Point = Point {x :: Int, y :: Int, label :: String} deriving (Show)

norm1 :: Point -> Int
norm1 Point {x, y} = abs x + abs y

describe :: Point -> String
describe Point {label, x = 0} = label ++ " on the axis"
describe Point {label} = label

mk :: Int -> Int -> Point
mk x y = Point {x, y, label = "made"}

main :: IO ()
main = do
  print (norm1 (Point 3 (-4) "p"))
  putStrLn (describe (Point 0 1 "a"))
  putStrLn (describe (Point 2 1 "b"))
  print (mk 5 6)
  let label = "changed"
      y = 9
  print ((mk 1 2) {label, y})
  print (case mk 7 8 of Point {y, x} -> (x, y))
  print ((\Point {x} -> x) (mk 10 11))
