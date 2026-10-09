-- Haskell 2010 Report 4.3.2: methods omitted from an instance take the class default (Eq via /=, Ord via <= or compare)
module Main (main) where

data T = A | B | C

tag :: T -> Int
tag A = 0
tag B = 1
tag C = 2

-- only (/=); (==) is the default
instance Eq T where
  x /= y = tag x /= tag y

-- only (<=); compare, <, >, >=, max and min are defaults
instance Ord T where
  x <= y = tag x <= tag y

data P = P Int Int

-- only (==)
instance Eq P where
  P a b == P c d = a == c && b == d

-- only compare
instance Ord P where
  compare (P a b) (P c d) = compare (a + b) (c + d)

unP :: P -> (Int, Int)
unP (P a b) = (a, b)

-- user class: an instance with an empty body inherits everything
class Describe a where
  describe :: a -> String
  describe _ = "thing"
  describeAll :: [a] -> String
  describeAll = concatMap describe

instance Describe T

instance Describe P where
  describe p = show (unP p)

main :: IO ()
main = do
  print ("eq default", A == A, A == B)
  print ("neq", A /= A, A /= B)
  print ("compare default", compare A B, compare B B, compare C B)
  print ("lt gt default", A < B, B < A, C > A, A > A)
  print ("ge default", A >= A, A >= B, C >= B)
  print ("max min default", tag (max A C), tag (min B C), tag (max B B))
  print ("maximum", tag (maximum [B, C, A]), tag (minimum [B, C, A]))
  print ("in list", [A, B] == [A, B], [A, C] < [B])
  print ("p neq default", P 1 2 /= P 1 2, P 1 2 /= P 2 1)
  print ("p operators default", P 1 2 < P 2 2, P 1 2 <= P 2 1, P 5 5 > P 1 1, P 0 0 >= P 0 1)
  print ("p max min default", unP (max (P 1 1) (P 3 3)), unP (min (P 1 1) (P 3 3)))
  print ("describe default", describe A, describeAll [A, B])
  print ("describe overridden", describe (P 1 2), describeAll [P 1 2, P 3 4])
