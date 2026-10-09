module Classes (Pretty(..)) where

class Pretty a where
  pretty :: a -> String
  prettyList :: [a] -> String
  prettyList xs = concat (map pretty xs)
