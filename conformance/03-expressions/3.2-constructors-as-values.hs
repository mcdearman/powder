-- Haskell 2010 Report 3.2: constructors (including constructor operators and special constructors) are values
module Main (main) where

infixl 6 :+:
infixl 7 :*:

data Shape = Circle Int | Rect Int Int
  deriving (Eq, Show)

data Expr = Int :+: Int | Int :*: Int
  deriving (Eq, Show)

main :: IO ()
main = do
  print ("map ctor", map Circle [1, 2])
  print ("zipWith ctor", zipWith Rect [1, 2] [3, 4])
  print ("partial ctor", map (Rect 0) [5, 6])
  print ("conop prefix", (:+:) 1 2, (:*:) 3 4)
  print ("conop infix", 1 :+: 2, 3 :*: 4)
  print ("conop equal", (:+:) 1 2 == 1 :+: 2)
  print ("uncurry conop", uncurry (:*:) (3, 4))
  print ("foldr cons", foldr (:) [] "abc")
  print ("map Just", map Just "ab")
  print ("either", map (either Circle (Rect 1)) [Left 1, Right 2])
  print ("compose", (Just . Circle) 3)
  print ("nullary", [False, True], [LT, EQ, GT])
  print ("unit/nil", (), [] :: [Int])
  print ("tuple ctor", (,) 'a' True)
  print ("nil in fold", foldr (\x acc -> x : acc) [] [1, 2, 3])
  print ("ctor list", map (\f -> f 2) [Circle, Rect 1])
  print ("maybe", maybe 0 (+ 1) (Just 5), maybe 0 (+ 1) Nothing)
  print ("backquoted", 1 `Rect` 2)
