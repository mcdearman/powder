module B where

infixl 6 <+>

data Item = Item { payload :: String } deriving Show

class Render a where
  render :: a -> String

instance Render Item where
  render (Item s) = "B.Item " ++ s

name :: String
name = "B.name"

value :: String
value = "B.value"

(<+>) :: Int -> Int -> Int
a <+> b = a - b

onlyB :: Int
onlyB = 2
