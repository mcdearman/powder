module Data.Semigroup
  ( Semigroup (..), NonEmpty (..), Min (..), Max (..), First (..), Last (..), Dual (..), Endo (..), All (..), Any (..), Sum (..), Product (..)
  )
where

import Data.Monoid hiding (First (..), Last (..))
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

-- The first of them, and the last: not Data.Monoid's, which are of Maybe.
newtype First a = First {getFirst :: a} deriving (Eq, Ord, Show, Read, Bounded)

instance Semigroup (First a) where
  a <> _ = a

newtype Last a = Last {getLast :: a} deriving (Eq, Ord, Show, Read, Bounded)

instance Semigroup (Last a) where
  _ <> b = b
