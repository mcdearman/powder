module Data.Traversable
  ( Traversable (..), for, forM, mapAccumL, mapAccumR
  )
where

import Control.Monad (forM)
import Data.List (mapAccumL, mapAccumR)
import Powder.Base

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for t f = traverse f t
