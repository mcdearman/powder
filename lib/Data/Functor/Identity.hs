module Data.Functor.Identity
  ( Identity (..)
  )
where

import Powder.Base

-- The functor that does nothing: a value, and no more.
newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Ord)

instance Show a => Show (Identity a) where
  showsPrec d (Identity x) = showParen (d > 10) (showString "Identity " . showsPrec 11 x)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= k = k a

instance Foldable Identity where
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
