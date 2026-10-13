{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingVia, DerivingStrategies #-}
-- Data.Coerce and DerivingVia: one type's values taken as another's that is kept the same way, and an instance that
-- is another type's taken so
module Main (main) where

import Data.Coerce (Coercible, coerce)
import Data.Monoid (Sum (..), Product (..), Any (..))
import Data.Ord (Down (..))
import Data.List (sort)

newtype Age = Age Int deriving (Show, Eq, Ord)

newtype Meters = Meters Double deriving (Show)

newtype Wrap a = Wrap a deriving (Show)

newtype Pair a b = Pair (a, b) deriving (Show)

-- The instances of the type named, not those of what the newtype is of.
newtype Total = Total Int
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Sum Int)

newtype Scale = Scale Int
  deriving stock (Show, Eq)
  deriving (Semigroup, Monoid) via (Product Int)

newtype Flag = Flag Bool
  deriving stock (Show)
  deriving (Semigroup, Monoid) via Any

newtype Backwards = Backwards Int
  deriving stock (Show, Eq)
  deriving (Ord) via (Down Int)

-- Of a data type that is not a newtype, by way of one that is kept as it is.
newtype Shown a = Shown a

instance Show a => Show (Shown a) where
  show (Shown a) = "<<" ++ show a ++ ">>"

newtype Tag = Tag String deriving (Show) via (Shown String)

ages :: [Int] -> [Age]
ages = coerce

total :: [Age] -> Int
total = sum . (coerce :: [Age] -> [Int])

unwrapAll :: Coercible a b => [a] -> [b]
unwrapAll = coerce

main :: IO ()
main = do
  print (coerce (3 :: Int) :: Age, coerce (Age 4) :: Int)
  print (ages [1, 2, 3], total [Age 10, Age 20])
  print (coerce (Just (Age 5)) :: Maybe Int, coerce [(Age 1, 'a')] :: [(Int, Char)])
  print (coerce (Wrap (Age 6)) :: Int, coerce (7 :: Int) :: Wrap (Wrap Age))
  print (coerce (Pair (1 :: Int, 'x')) :: (Age, Char), coerce (Meters 2.5) + (1 :: Double))
  print ((coerce (succ :: Int -> Int) :: Age -> Age) (Age 8))
  print (getSum (coerce (Age 9) :: Sum Int), (unwrapAll [Age 1, Age 2] :: [Int]))
  print (Total 2 <> Total 3, mempty :: Total, mconcat [Total 1, Total 2, Total 3])
  print (Scale 2 <> Scale 3, mempty :: Scale)
  print (Flag False <> Flag True, mempty :: Flag)
  print (sort [Backwards 1, Backwards 3, Backwards 2], Backwards 1 < Backwards 2)
  print (Tag "t")
