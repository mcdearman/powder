module A where

import Data.List (sort)

infixr 5 |+|

type Number = Int

data Colour = Red | Green deriving (Show, Eq)

newtype Wrap = Wrap { unwrap :: Int } deriving Show

class Describe a where
  describe :: a -> String

instance Describe Colour where
  describe c = "colour " ++ show c

instance Describe Wrap where
  describe (Wrap n) = "wrap " ++ show n

aValue :: Number
aValue = 42

aFunction :: Int -> Int
aFunction = (* 3)

(|+|) :: Int -> Int -> Int
a |+| b = a * 10 + b

sortedInA :: [Int]
sortedInA = sort [3, 1, 2]
