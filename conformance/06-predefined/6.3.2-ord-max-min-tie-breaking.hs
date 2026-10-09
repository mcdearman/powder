-- Haskell 2010 Report 6.3.2: default max x y | x <= y = y | otherwise = x; min x y | x <= y = x | otherwise = y
-- For equal (but distinguishable) arguments max returns the second and min the first.

-- compared on the key only; the tag makes equal values distinguishable
data Tagged = Tagged Int Char deriving Show

instance Eq Tagged where
  Tagged a _ == Tagged b _ = a == b

instance Ord Tagged where
  Tagged a _ <= Tagged b _ = a <= b

-- the same, but defined through compare
data Keyed = Keyed Int Char deriving Show

instance Eq Keyed where
  Keyed a _ == Keyed b _ = a == b

instance Ord Keyed where
  compare (Keyed a _) (Keyed b _) = compare a b

-- an instance overriding max and min: the overrides are used
newtype Weird = Weird Int deriving (Eq, Show)

instance Ord Weird where
  Weird a <= Weird b = a <= b
  max _ _ = Weird 100
  min _ _ = Weird (-100)

main :: IO ()
main = do
  print ("max equal keys (<=)", max (Tagged 1 'a') (Tagged 1 'b'))
  print ("min equal keys (<=)", min (Tagged 1 'a') (Tagged 1 'b'))
  print ("max distinct keys (<=)", max (Tagged 2 'a') (Tagged 1 'b'), max (Tagged 1 'a') (Tagged 2 'b'))
  print ("min distinct keys (<=)", min (Tagged 2 'a') (Tagged 1 'b'), min (Tagged 1 'a') (Tagged 2 'b'))
  print ("max equal keys (compare)", max (Keyed 1 'a') (Keyed 1 'b'))
  print ("min equal keys (compare)", min (Keyed 1 'a') (Keyed 1 'b'))
  print ("max distinct keys (compare)", max (Keyed 2 'a') (Keyed 1 'b'), max (Keyed 1 'a') (Keyed 2 'b'))
  print ("min distinct keys (compare)", min (Keyed 2 'a') (Keyed 1 'b'), min (Keyed 1 'a') (Keyed 2 'b'))
  print ("equal keys compare EQ", compare (Keyed 1 'a') (Keyed 1 'b'), compare (Tagged 1 'a') (Tagged 1 'b'))
  print ("equal keys both <= and >=", Tagged 1 'a' <= Tagged 1 'b', Tagged 1 'a' >= Tagged 1 'b', Tagged 1 'a' < Tagged 1 'b')
  print ("overridden max/min", max (Weird 1) (Weird 2), min (Weird 1) (Weird 2))
  print ("overrides do not affect comparison", Weird 1 < Weird 2, compare (Weird 2) (Weird 1))
  print ("standard types", max 3 (7 :: Int), min 3 (7 :: Int), max 'a' 'b', min "ab" "abc", max (1 :: Int, 'x') (1, 'y'))
  print ("max/min as binary functions", foldr max 0 [3, 9, 2 :: Int], foldr min 100 [3, 9, 2 :: Int], 3 `max` 4 `max` (1 :: Int))
