{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeOperators #-}
module Sums ((:+:) (..), (:*:) (..), type (+), type (~>), swap, both) where

infixr 5 :+:

infixr 6 :*:

data a :+: b = L a | R b deriving (Show, Eq)

data a :*: b = a :*: b deriving (Show, Eq)

type a + b = Either a b

type a ~> b = a -> b

instance Functor ((:+:) a) where
  fmap _ (L a) = L a
  fmap f (R b) = R (f b)

swap :: a :+: b -> b :+: a
swap (L a) = R a
swap (R b) = L b

both :: a -> b -> a :*: b
both = (:*:)
