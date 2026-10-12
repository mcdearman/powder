module Data.Foldable
  ( Foldable (..), foldrM, foldlM, traverse_, for_, sequenceA_, asum, mapM_, forM_, sequence_, msum,
    concat, concatMap, and, or, any, all, maximumBy, minimumBy, notElem, find
  )
where

import Control.Monad (forM_, msum)
import Powder.Base

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z t = foldl (\k x acc -> f x acc >>= k) return t z

foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z t = foldr (\x k acc -> f acc x >>= k) return t z

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ t f = traverse_ f t

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty
