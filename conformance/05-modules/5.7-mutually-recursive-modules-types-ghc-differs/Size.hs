module Size (Size(..)) where

class Size a where
  size :: a -> Int

instance Size a => Size [a] where
  size xs = sum (map size xs)
