module Shape (Measure(..), Sq(..), report) where

class Measure a where
  area :: a -> Int
  label :: a -> String
  label _ = "shape"

newtype Sq = Sq Int

report :: Measure a => a -> String
report x = label x ++ ":" ++ show (area x)
