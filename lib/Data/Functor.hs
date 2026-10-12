module Data.Functor
  ( Functor (..), ($>), (<$>), (<&>), void
  )
where

import Powder.Base

infixl 4 $>
infixl 1 <&>

($>) :: Functor f => f a -> b -> f b
m $> x = x <$ m

(<&>) :: Functor f => f a -> (a -> b) -> f b
m <&> f = fmap f m

void :: Functor f => f a -> f ()
void m = () <$ m
