module Inner (innerValue, innerFn, In(..), Tag(..)) where

data In = InA | InB deriving Show

class Tag a where
  tag :: a -> Char

instance Tag In where
  tag InA = 'a'
  tag InB = 'b'

innerValue :: Int
innerValue = 1

innerFn :: Int -> Int
innerFn = (+ 100)
