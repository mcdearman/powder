module Data.Monoid
  ( Monoid (..), (<>), Dual (..), Endo (..), All (..), Any (..), Sum (..), Product (..), First (..), Last (..)
  )
where

import Powder.Base

newtype Dual a = Dual {getDual :: a} deriving (Eq, Ord, Show, Read, Bounded)

instance Semigroup a => Semigroup (Dual a) where
  Dual a <> Dual b = Dual (b <> a)

instance Monoid a => Monoid (Dual a) where
  mempty = Dual mempty

newtype All = All {getAll :: Bool} deriving (Eq, Ord, Show, Read, Bounded)

instance Semigroup All where
  All a <> All b = All (a && b)

instance Monoid All where
  mempty = All True

newtype Any = Any {getAny :: Bool} deriving (Eq, Ord, Show, Read, Bounded)

instance Semigroup Any where
  Any a <> Any b = Any (a || b)

instance Monoid Any where
  mempty = Any False

newtype Sum a = Sum {getSum :: a} deriving (Eq, Ord, Show, Read, Bounded)

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

instance Functor Sum where
  fmap f (Sum a) = Sum (f a)

newtype Product a = Product {getProduct :: a} deriving (Eq, Ord, Show, Read, Bounded)

instance Num a => Semigroup (Product a) where
  Product a <> Product b = Product (a * b)

instance Num a => Monoid (Product a) where
  mempty = Product 1

instance Functor Product where
  fmap f (Product a) = Product (f a)

-- The first of them that is something.
newtype First a = First {getFirst :: Maybe a} deriving (Eq, Ord, Show, Read)

instance Semigroup (First a) where
  First Nothing <> b = b
  a <> _ = a

instance Monoid (First a) where
  mempty = First Nothing

-- The last of them that is something.
newtype Last a = Last {getLast :: Maybe a} deriving (Eq, Ord, Show, Read)

instance Semigroup (Last a) where
  a <> Last Nothing = a
  _ <> b = b

instance Monoid (Last a) where
  mempty = Last Nothing
