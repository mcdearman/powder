{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
-- RecordWildCards: `..` among a record's fields is every field not named, each as the variable of its name
module Main (main) where

data Point = Point {x :: Int, y :: Int, label :: String} deriving (Show)

data Shape
  = Circle {centre :: Point, radius :: Int}
  | Box {corner :: Point, width :: Int, height :: Int}
  deriving (Show)

norm1 :: Point -> Int
norm1 Point {..} = abs x + abs y

describe :: Point -> String
describe Point {x = 0, ..} = label ++ " on the axis at " ++ show y
describe Point {label, ..} = label ++ " at " ++ show (x, y)

area :: Shape -> Int
area Circle {..} = 3 * radius * radius
area Box {..} = width * height

mk :: Int -> Int -> Point
mk x y = Point {..}
  where
    label = "made"

moved :: Int -> Point -> Point
moved dx Point {..} = Point {x = x + dx, ..}

main :: IO ()
main = do
  print (norm1 (Point 3 (-4) "p"))
  putStrLn (describe (Point 0 1 "a"))
  putStrLn (describe (Point 2 1 "b"))
  print (map area [Circle (Point 0 0 "c") 2, Box (Point 1 1 "b") 3 4])
  print (mk 5 6)
  print (moved 10 (mk 1 2))
  let x = 7
      y = 8
      label = "let"
  print Point {..}
  print (case mk 1 2 of Point {..} -> x + y)
  print ((\Point {..} -> label) (mk 0 0))
  let Point {..} = mk 20 30 in print (x, y, label)
  print [radius | Circle {..} <- [Circle (mk 0 0) 5, Box (mk 0 0) 1 1, Circle (mk 0 0) 6]]
  r <- return (Box (mk 0 0) 2 3)
  case r of
    Box {corner = Point {..}, ..} -> print (x, y, width, height)
    _ -> return ()
