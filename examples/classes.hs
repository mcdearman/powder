-- Type classes: a class with a default and a superclass, instances with
-- contexts, derived instances, and a class of type constructors.

data Shape = Circle Int | Rect Int Int deriving (Eq, Ord, Show)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

class Show a => Describe a where
  describe :: a -> String
  describe x = "<" ++ show x ++ ">"
  name :: a -> String

instance Describe Shape where
  name (Circle _) = "circle"
  name (Rect _ _) = "rectangle"

instance Describe Bool where
  describe b = if b then "yes" else "no"
  name _ = "bool"

instance Describe a => Describe [a] where
  describe xs = unwords (map describe xs)
  name xs = "list of " ++ show (length xs)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x t@(Node l y r) = case compare x y of
  LT -> Node (insert x l) y r
  GT -> Node l y (insert x r)
  EQ -> t

toList :: Tree a -> [a]
toList Leaf = []
toList (Node l x r) = toList l ++ [x] ++ toList r

-- No signature: the context is inferred.
total xs = sum xs + fromIntegral (length xs)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

data Day = Mon | Tue | Wed | Thu | Fri deriving (Eq, Ord, Show, Enum, Bounded)

main :: IO ()
main = do
  let shapes = [Rect 2 3, Circle 1, Rect 1 5, Circle 1]
      tree = foldr insert Leaf shapes
  print (toList tree)
  print (fmap name tree)
  putStrLn (describe shapes)
  putStrLn (describe [True, False] ++ ", a " ++ name [True, False])
  print (Circle 1 == Circle 1, Circle 1 < Rect 0 0, maximum shapes)
  print (total [1, 2, 3], total [4 :: Int ..] `seq` ())
  print (safeDiv 7 2 >>= safeDiv 100, safeDiv 1 0 >>= safeDiv 100)
  print (do { x <- [1, 2, 3]; y <- "ab"; return (x, y) })
  print (Just (-3), [Left 1, Right "two"], compare 'a' 'b')
  print (minBound :: Int, [False ..], show 'x' ++ show "q\"\n")
  print ([Tue ..], [Mon, Wed ..], [Fri, Thu .. Tue], succ Mon, fromEnum Thu, [minBound .. maxBound :: Day])
