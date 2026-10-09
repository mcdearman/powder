-- A newtype, an operator with a fixity of its own, and an instance, for
-- another module to use.
module Parts.Wrap (Age (..), (<+>), older) where

infixr 5 <+>

newtype Age = Age Int deriving (Eq, Show)

instance Num Age where
  Age a + Age b = Age (a + b)
  Age a * Age b = Age (a * b)
  negate (Age a) = Age (negate a)
  abs (Age a) = Age (abs a)
  signum (Age a) = Age (signum a)
  fromInteger n = Age (fromInteger n)

(<+>) :: [a] -> [a] -> [a]
xs <+> ys = xs ++ ys

older :: Age -> Age -> Bool
older (Age a) (Age b) = a > b
