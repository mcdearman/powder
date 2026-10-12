module Data.Semigroup
  ( Semigroup (..), Min (..), Max (..), Dual (..), Endo (..), All (..), Any (..), Sum (..), Product (..)
  )
where

import Data.Monoid
import Powder.Base

newtype Min a = Min {getMin :: a} deriving (Eq, Ord, Show, Read, Bounded)

instance Ord a => Semigroup (Min a) where
  Min a <> Min b = Min (min a b)

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound

newtype Max a = Max {getMax :: a} deriving (Eq, Ord, Show, Read, Bounded)

instance Ord a => Semigroup (Max a) where
  Max a <> Max b = Max (max a b)

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound
