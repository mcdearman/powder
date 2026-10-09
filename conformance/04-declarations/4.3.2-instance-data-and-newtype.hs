-- Haskell 2010 Report 4.3.2: instance declarations for data and newtype types; methods by clauses, guards and where
module Main (main) where

class Pretty a where
  pretty :: a -> String
  prettyList :: [a] -> String
  prettyList xs = "[" ++ concatMap pretty xs ++ "]"

data Color = Red | Green | Blue

data Shape = Circle Int | Rect Int Int

data Tree a = Leaf | Node (Tree a) a (Tree a)

data Pair a b = Pair a b

newtype Celsius = Celsius Int

newtype Wrapped a = Wrapped a

-- one clause per constructor
instance Pretty Color where
  pretty Red = "R"
  pretty Green = "G"
  pretty Blue = "B"

-- guards and where inside instance methods
instance Pretty Shape where
  pretty (Circle r)
    | r <= 0 = "point"
    | otherwise = "circle" ++ show r
  pretty (Rect w h)
    | w == h = "square" ++ side
    | otherwise = "rect" ++ side ++ "x" ++ show h
    where
      side = show w

-- context on a parameterised type
instance Pretty a => Pretty (Tree a) where
  pretty Leaf = "."
  pretty (Node l v r) = "(" ++ pretty l ++ pretty v ++ pretty r ++ ")"

instance (Pretty a, Pretty b) => Pretty (Pair a b) where
  pretty (Pair a b) = "<" ++ pretty a ++ "," ++ pretty b ++ ">"

instance Pretty Celsius where
  pretty (Celsius n) = show n ++ "C"
  prettyList cs = "temps:" ++ concatMap pretty cs

instance Pretty a => Pretty (Wrapped a) where
  pretty (Wrapped x) = "{" ++ pretty x ++ "}"

main :: IO ()
main = do
  print ("color", map pretty [Red, Green, Blue])
  print ("shape", map pretty [Circle 0, Circle 2, Rect 3 3, Rect 2 5])
  print ("tree", pretty (Node (Node Leaf Red Leaf) Green Leaf))
  print ("tree of shapes", pretty (Node Leaf (Rect 1 1) Leaf))
  print ("pair", pretty (Pair Blue (Circle 1)))
  print ("nested", pretty (Pair (Pair Red Red) (Node Leaf Blue Leaf)))
  print ("newtype", pretty (Celsius 21))
  print ("newtype wrapped", pretty (Wrapped (Wrapped Green)))
  print ("default prettyList", prettyList [Red, Blue])
  print ("overridden prettyList", prettyList [Celsius 1, Celsius 2])
  print ("prettyList at tree", prettyList [Leaf, Node Leaf Red Leaf])
