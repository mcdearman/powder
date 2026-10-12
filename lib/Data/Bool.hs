module Data.Bool
  ( Bool (..), (&&), (||), not, otherwise, bool
  )
where

-- What a Bool chooses between: the first if it is False.
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True = t
