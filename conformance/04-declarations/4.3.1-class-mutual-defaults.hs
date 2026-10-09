-- Haskell 2010 Report 4.3.1: default methods defined in terms of each other (as Eq's == and /=)
module Main (main) where

class MyEq a where
  eq, neq :: a -> a -> Bool
  eq x y = not (neq x y)
  neq x y = not (eq x y)

-- defines only eq
instance MyEq Int where
  eq x y = x == y

-- defines only neq
instance MyEq Bool where
  neq True False = True
  neq False True = True
  neq _ _ = False

-- defines both
instance MyEq Char where
  eq a b = a == b
  neq a b = a /= b

instance MyEq a => MyEq [a] where
  eq [] [] = True
  eq (x : xs) (y : ys) = eq x y && eq xs ys
  eq _ _ = False

instance (MyEq a, MyEq b) => MyEq (a, b) where
  neq (a, b) (c, d) = neq a c || neq b d

class Parity a where
  isEven, isOdd :: a -> Bool
  isEven = not . isOdd
  isOdd = not . isEven

instance Parity Int where
  isEven n = n `mod` 2 == 0

instance Parity Integer where
  isOdd n = n `mod` 2 == 1

main :: IO ()
main = do
  print ("int eq", eq (1 :: Int) 1, eq (1 :: Int) 2)
  print ("int neq default", neq (1 :: Int) 1, neq (1 :: Int) 2)
  print ("bool neq", neq True False, neq True True)
  print ("bool eq default", eq True False, eq False False)
  print ("char", eq 'a' 'a', neq 'a' 'a')
  print ("list eq", eq [1, 2 :: Int] [1, 2], eq [1, 2 :: Int] [1])
  print ("list neq default", neq "ab" "ab", neq "ab" "ac")
  print ("pair neq", neq (1 :: Int, True) (1, True), neq (1 :: Int, True) (1, False))
  print ("pair eq default", eq ('a', [True]) ('a', [True]), eq ('a', [True]) ('b', [True]))
  print ("parity int", isEven (4 :: Int), isOdd (4 :: Int))
  print ("parity integer", isEven (7 :: Integer), isOdd (7 :: Integer))
