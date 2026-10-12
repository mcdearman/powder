module Control.Applicative
  ( Applicative (..), Alternative (..), (<$>), (<$), (<**>), liftA, liftA3, optional
  )
where

import Powder.Base

infixl 4 <**>

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\x f -> f x)

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f m = pure f <*> m

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing
