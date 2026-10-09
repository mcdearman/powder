-- Haskell 2010 Report 11.1: a derived Ord instance provides compare, (<), (<=), (>=), (>), max and min, all consistent
module Main (main) where

data T = A | B Int | C Char Bool | D [T] deriving (Eq, Ord, Show)

values :: [T]
values = [A, B (-1), B 0, B 7, C 'a' False, C 'a' True, C 'b' False, D [], D [A], D [A, B 0], D [B 0]]

row :: T -> T -> IO ()
row x y = print (x, y, compare x y, [x < y, x <= y, x >= y, x > y], max x y, min x y)

consistent :: T -> T -> Bool
consistent x y = case compare x y of
  LT -> x < y && x <= y && not (x >= y) && not (x > y) && not (x == y) && max x y == y && min x y == x
  EQ -> not (x < y) && x <= y && x >= y && not (x > y) && x == y && max x y == x && min x y == x
  GT -> not (x < y) && not (x <= y) && x >= y && x > y && not (x == y) && max x y == x && min x y == y

main :: IO ()
main = do
  row A A
  row A (B 0)
  row (B 0) A
  row (B 0) (B 7)
  row (B 7) (B (-1))
  row (C 'a' True) (C 'b' False)
  row (C 'a' True) (C 'a' False)
  row (D []) (C 'z' True)
  row (D [A]) (D [A, B 0])
  row (D [B 0]) (D [A, B 0])
  row (D [D [D []]]) (D [D [D [A]]])
  print ("values are strictly increasing", and (zipWith (<) values (tail values)))
  print ("consistent on all pairs", and [consistent x y | x <- values, y <- values])
  print ("compare table", [[compare x y | y <- values] | x <- values])
  print ("methods as functions", zipWith max values (reverse values) == zipWith (flip max) values (reverse values))
  print ("maximum/minimum", maximum values, minimum values, maximum (reverse values), minimum (reverse values))
  print ("sections", filter (< C 'a' True) values, filter (>= D [A]) values)
  print ("max/min chains", foldr1 max values, foldr1 min values, max A (max (B 1) (C 'c' True)), min (D []) (min (B 1) (C 'c' True)))
  print ("comparing", compare (compare A (B 0)) (compare (B 0) A), compare (A < B 0) (B 0 < A))
