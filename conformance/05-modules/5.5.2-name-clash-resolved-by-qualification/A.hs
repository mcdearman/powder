module A where

infixl 6 <+>

data Item = Item { payload :: Int } deriving Show

class Render a where
  render :: a -> String

instance Render Item where
  render (Item n) = "A.Item " ++ show n

name :: String
name = "A.name"

value :: String
value = "A.value"

(<+>) :: Int -> Int -> Int
a <+> b = a + b

onlyA :: Int
onlyA = 1
