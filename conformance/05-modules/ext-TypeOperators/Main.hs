{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeOperators, ExplicitNamespaces #-}
-- TypeOperators: a type whose name is an operator, written between what it is applied to
module Main (main) where

import Sums (type (:+:) (..), type (:*:) (..), type (+), type (~>), swap, both)

infixr 5 +++

data a +++ b = Nope | a :+++ b deriving (Show)

type Endo a = a ~> a

newtype f :.: g = Compose (f (g Int))

describe :: Int :+: String -> String
describe (L n) = "left " ++ show n
describe (R s) = "right " ++ s

-- Tighter than the arrow, and grouped by their fixities.
mixed :: Int :+: Bool :*: Char -> String
mixed (L n) = show n
mixed (R (b :*: c)) = show b ++ [c]

twice :: Endo Int -> Int ~> Int
twice f = f . f

sumOf :: Int + Int -> Int
sumOf = either id negate

nested :: Int +++ Bool +++ Char
nested = 1 :+++ (True :+++ 'c')

main :: IO ()
main = do
  putStrLn (describe (L 1) ++ ", " ++ describe (R "two"))
  putStrLn (mixed (L 5) ++ " " ++ mixed (R (True :*: 'x')))
  print (twice (+ 3) 1, sumOf (Left 4), sumOf (Right 4))
  print (swap (L 'a' :: Char :+: Int), swap (R 2 :: Char :+: Int))
  print (both (1 :: Int) 'b', nested)
  print (case Compose (Just [1, 2, 3]) :: Maybe :.: [] of Compose m -> fmap sum m)
  print (fmap (+ 1) (R 1 :: Char :+: Int))
