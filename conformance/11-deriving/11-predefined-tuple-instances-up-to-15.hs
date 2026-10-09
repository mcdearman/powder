-- Haskell 2010 Report 11 (and 6.1.4): Eq, Ord, Bounded, Read and Show instances are provided for tuples; sizes up to 15 are tested
module Main (main) where

type I7 = (Int, Int, Int, Int, Int, Int, Int)

type I15 = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

type B15 = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

type M15 = (Int, Char, Bool, String, Double, (), Ordering, Maybe Int, [Int], Integer, Either Int Char, (Int, Int), Float, [Char], Bool)

a15 :: I15
a15 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

b15 :: I15
b15 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16)

c15 :: I15
c15 = (1, 2, 3, 4, 5, 6, 7, 0, 99, 99, 99, 99, 99, 99, 99)

m15 :: M15
m15 = (-1, 'c', True, "s", 2.5, (), GT, Just (-2), [3], 2 ^ (70 :: Int), Right 'r', (4, 5), 0.5, "", False)

main :: IO ()
main = do
  print ("Eq 15", a15 == a15, a15 == b15, a15 /= b15, a15 /= a15)
  print ("Ord 15 last component", compare a15 b15, a15 < b15, a15 <= b15, a15 > b15, a15 >= b15)
  print ("Ord 15 earlier component decides", compare a15 c15, c15 < b15, max a15 c15 == a15, min a15 c15 == c15)
  print a15
  print m15
  print ("Show 15 at precedence 11", showsPrec 11 a15 "")
  print ("Read 15", (read "(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)" :: I15) == a15)
  print ("Read 15 with spaces", (read " ( 1 , 2,3,4,5,6,7,8,9,10,11,12,13,14 , 16 ) " :: I15) == b15)
  print ("Read . Show 15", (read (show a15) :: I15) == a15, (read (show m15) :: M15) == m15)
  print ("Bounded 15", minBound :: B15)
  print ("Bounded 15 max", maxBound :: B15)
  print ("Bounded 15 Int", (minBound :: I15) < a15, (maxBound :: I15) > a15)
  print ("Eq/Ord 7", (1, 2, 3, 4, 5, 6, 7) == ((1, 2, 3, 4, 5, 6, 7) :: I7), compare ((1, 2, 3, 4, 5, 6, 7) :: I7) (1, 2, 3, 4, 5, 6, 8))
  print ("Read/Show 7", read "(1,2,3,4,5,6,7)" :: I7, minBound :: (Bool, (), Ordering, Bool, (), Ordering, Bool))
  print ("size 2", (1 :: Int, 'a') < (1, 'b'), read "(1,'a')" :: (Int, Char), maxBound :: (Bool, Ordering))
  print ("size 3", (1 :: Int, 'a', True) > (1, 'a', False), read "(1,'a',True)" :: (Int, Char, Bool), minBound :: (Bool, Ordering, ()))
  print ("size 4", ('a', 'b', 'c', 'd') == ('a', 'b', 'c', 'd'), compare (1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int) (1, 2, 3, 3), read "(1,2,3,4)" :: (Int, Int, Int, Int))
  print ("size 5", (1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int, 5 :: Int) < (1, 2, 3, 4, 6), read "((),(),(),(),())" :: ((), (), (), (), ()))
  print ("size 6", maxBound :: (Bool, Bool, Bool, Bool, Bool, Bool), read "(1,'x',\"s\",2.5,True,LT)" :: (Int, Char, String, Double, Bool, Ordering))
  print ("nested", ((1 :: Int, 'a'), ('b', 2 :: Int)) < ((1, 'a'), ('b', 3)), read "((1,2),(3,(4,5)))" :: ((Int, Int), (Int, (Int, Int))))
  print ("lexicographic not componentwise", (1 :: Int, 9 :: Int) < (2, 0), (2 :: Int, 0 :: Int) > (1, 9), max (1 :: Int, 'z') (2, 'a'))
  print ("comparison is lazy to the right", (1 :: Int, undefined :: Int) == (2, undefined), compare (1 :: Int, undefined :: Int) (2, undefined))
