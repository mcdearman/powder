module Data.Ord
  ( Ord (..), Ordering (..), comparing, clamp, Down (..)
  )
where

import Powder.Base

comparing :: Ord a => (b -> a) -> b -> b -> Ordering
comparing f x y = compare (f x) (f y)

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)

-- A value whose order is the other way round.
newtype Down a = Down {getDown :: a} deriving (Eq)

instance Ord a => Ord (Down a) where
  compare (Down x) (Down y) = compare y x

instance Show a => Show (Down a) where
  showsPrec d (Down x) = showParen (d > 10) (showString "Down " . showsPrec 11 x)

instance Bounded a => Bounded (Down a) where
  minBound = Down maxBound
  maxBound = Down minBound

instance Functor Down where
  fmap f (Down a) = Down (f a)

instance Semigroup a => Semigroup (Down a) where
  Down a <> Down b = Down (a <> b)

instance Monoid a => Monoid (Down a) where
  mempty = Down mempty
