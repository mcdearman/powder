{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE KindSignatures, StandaloneKindSignatures, ExplicitForAll #-}
-- KindSignatures and StandaloneKindSignatures: the kind of a type variable said where it is named, of a type where
-- it is written, and of a type or a class apart from its declaration
module Main (main) where

import Data.Kind (Type)

data Wrap (f :: Type -> Type) (a :: Type) = Wrap (f a)

newtype Fix (f :: * -> *) = Fix (f (Fix f))

data ListF a r = Nil | Cons a r

type Pair :: Type -> Type
type Pair a = (a, a)

type App :: (Type -> Type) -> Type -> Type
newtype App f a = App (f a)

type Shown :: Type -> Type
data Shown a = Shown String

type Container :: (Type -> Type) -> Type
data Container f = Container (f Int)

class Functor' (f :: Type -> Type) where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
  fmap' _ Nothing = Nothing
  fmap' g (Just x) = Just (g x)

instance Functor' (ListF a) where
  fmap' _ Nil = Nil
  fmap' g (Cons a r) = Cons a (g r)

type Showy :: Type -> Type -> Type
type Showy a b = Either a b

apply :: forall (f :: Type -> Type) (a :: Type) b. Functor' f => (a -> b) -> f a -> f b
apply = fmap'

identity :: forall (a :: Type). a -> a
identity x = x

unwrap :: Wrap Maybe Int -> Maybe (Int :: Type)
unwrap (Wrap m) = m

cata :: Functor' f => (f a -> a) -> Fix f -> a
cata alg (Fix x) = alg (fmap' (cata alg) x)

fromList :: [a] -> Fix (ListF a)
fromList = foldr (\a r -> Fix (Cons a r)) (Fix Nil)

total :: ListF Int Int -> Int
total Nil = 0
total (Cons a r) = a + r

main :: IO ()
main = do
  print (unwrap (Wrap (Just 3)))
  print (cata total (fromList [1, 2, 3, 4]))
  print (apply (+ 1) (Just (1 :: Int)), identity 'i')
  print (fst ((1, 2) :: Pair Int), case App (Just 'a') of App m -> m)
  print (case Container [1, 2, 3] of Container xs -> sum xs, case Shown "s" :: Shown Bool of Shown s -> s)
  print (Left 1 :: Showy Int Char)
