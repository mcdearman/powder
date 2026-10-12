module Control.Monad
  ( Functor (..), Applicative (..), Monad (..), MonadFail (..), MonadPlus (..), mapM, mapM_, forM, forM_, sequence, sequence_, (=<<), (>=>), (<=<),
    forever, void, join, msum, filterM, mapAndUnzipM, zipWithM, zipWithM_, foldM, foldM_,
    replicateM, replicateM_, guard, when, unless, liftM, liftM2, liftM3, liftM4, liftM5, ap
  )
where

import Data.Functor (void)
import Powder.Base

infixr 1 >=>, <=<

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
  mzero = empty
  mplus = (<|>)

instance MonadPlus Maybe

instance MonadPlus []

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM xs f = mapM f xs

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ xs f = mapM_ f xs

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
g <=< f = f >=> g

forever :: Monad m => m a -> m b
forever a = a >> forever a

join :: Monad m => m (m a) -> m a
join m = m >>= id

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = foldr mplus mzero

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x : xs) = do
  keep <- p x
  rest <- filterM p xs
  return (if keep then x : rest else rest)

mapAndUnzipM :: Monad m => (a -> m (b, c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs = mapM f xs >>= return . unzip

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence (zipWith f xs ys)

zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a [] = return a
foldM f a (x : xs) = f a x >>= \y -> foldM f y xs

foldM_ :: Monad m => (a -> b -> m a) -> a -> [b] -> m ()
foldM_ f a xs = foldM f a xs >> return ()

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n x = sequence (replicate n x)

replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n x = sequence_ (replicate n x)

guard :: MonadPlus m => Bool -> m ()
guard True = return ()
guard False = mzero

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= \x -> return (f x)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m n = m >>= \x -> n >>= \y -> return (f x y)

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f m n o = m >>= \x -> n >>= \y -> o >>= \z -> return (f x y z)

liftM4 :: Monad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftM4 f m n o p = m >>= \x -> n >>= \y -> o >>= \z -> p >>= \w -> return (f x y z w)

liftM5 :: Monad m => (a -> b -> c -> d -> e -> g) -> m a -> m b -> m c -> m d -> m e -> m g
liftM5 f m n o p q = m >>= \x -> n >>= \y -> o >>= \z -> p >>= \w -> q >>= \v -> return (f x y z w v)

ap :: Monad m => m (a -> b) -> m a -> m b
ap = liftM2 id
