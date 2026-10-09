module Super (Super(..)) where

class Super a where
  base :: a -> Int
  twiceBase :: a -> Int
  twiceBase x = 2 * base x

instance Super a => Super [a] where
  base = sum . map base
