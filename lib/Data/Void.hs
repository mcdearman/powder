{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE EmptyCase #-}
-- The type with no values, and what follows from having one.
module Data.Void (Void, absurd, vacuous) where

data Void

instance Eq Void where
  _ == _ = True

instance Ord Void where
  compare _ _ = EQ

instance Show Void where
  showsPrec _ v = absurd v

instance Semigroup Void where
  a <> _ = a

-- Anything at all, since there is nothing to give it.
absurd :: Void -> a
absurd v = case v of {}

vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd
