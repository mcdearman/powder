-- Haskell 2010 Report 11.1: derived Eq and Ord for a sum, for a parameterised data type and for a
-- parameterised newtype; constructors compare in declaration order, fields left to right
module Main (main) where

data Point = Point Int Int | Special deriving (Eq, Ord)

data Wrapper a = Wrapper a deriving (Eq, Ord)

newtype Wrapped a = Wrapped a deriving (Eq, Ord)

main :: IO ()
main = do
  let p1 = Point 1 1
      p2 = Point 1 2
      p3 = Special
      w1 = Wrapper "abc"
      w2 = Wrapper "def"
      n1 = Wrapped "abc"
      n2 = Wrapped "def"
  print ("point", p1 == p1, compare p1 p1, p1 == p2, compare p1 p2, p1 == p3, compare p1 p3)
  print ("special", compare p3 p1, p3 == p3, p3 > p2, max p1 p3 == p3)
  print ("wrapper", w1 == w1, compare w1 w1, w1 == w2, compare w1 w2, compare w2 w1)
  print ("newtype", n1 == n1, compare n1 n1, n1 == n2, compare n1 n2, n2 >= n1)
  print ("second field decides", Point 2 0 > Point 1 9, Point 1 0 < Point 1 9, min p1 p2 == p1)
  print ("nested", Wrapper (Wrapped p1) < Wrapper (Wrapped p2), Wrapped (Wrapper p3) == Wrapped (Wrapper p3))
