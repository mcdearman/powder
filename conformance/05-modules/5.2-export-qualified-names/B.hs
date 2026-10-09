module B where

newtype Box = Box { unBox :: Int } deriving Show

class Label a where
  label :: a -> String

instance Label Box where
  label (Box n) = "box#" ++ show n
