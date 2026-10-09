-- Haskell 2010 Report 6.3.2: class Ord; defining only compare gives <, <=, >, >=, max, min by the defaults
data Version = Version Int Int deriving (Eq, Show)

instance Ord Version where
  compare (Version a b) (Version c d) = case compare a c of
    EQ -> compare b d
    other -> other

-- reversed ordering on the wrapped Int
newtype Down = Down Int deriving (Eq, Show)

instance Ord Down where
  compare (Down a) (Down b) = compare b a

v10, v12, v20 :: Version
v10 = Version 1 0
v12 = Version 1 2
v20 = Version 2 0

insertSorted :: Ord a => a -> [a] -> [a]
insertSorted x [] = [x]
insertSorted x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insertSorted x ys

sortAll :: Ord a => [a] -> [a]
sortAll = foldr insertSorted []

main :: IO ()
main = do
  print ("compare", compare v10 v12, compare v12 v10, compare v12 v12, compare v20 v12)
  print ("<", v10 < v12, v12 < v10, v12 < v12)
  print ("<=", v10 <= v12, v12 <= v10, v12 <= v12)
  print (">", v10 > v12, v12 > v10, v12 > v12)
  print (">=", v10 >= v12, v12 >= v10, v12 >= v12)
  print ("max", max v10 v12, max v12 v10, max v20 v20)
  print ("min", min v10 v12, min v12 v10, min v20 v20)
  print ("sorted", sortAll [v20, v10, v12, v10])
  print ("Down compare", compare (Down 1) (Down 2), compare (Down 2) (Down 1), compare (Down 3) (Down 3))
  print ("Down operators", Down 1 < Down 2, Down 1 > Down 2, Down 1 >= Down 1, Down 5 <= Down 4)
  print ("Down max/min", max (Down 1) (Down 2), min (Down 1) (Down 2))
  print ("Down sorted", sortAll (map Down [3, 1, 2]))
  print ("through list instance", [Down 1, Down 2] < [Down 1, Down 3], compare [v10] [v10, v12])
  print ("through tuple and Maybe", (v10, Down 1) < (v10, Down 0), Just v20 > Just v12, Nothing < Just v10)
  print ("maximum/minimum", maximum [v12, v20, v10], minimum [Down 1, Down 9, Down 4])
