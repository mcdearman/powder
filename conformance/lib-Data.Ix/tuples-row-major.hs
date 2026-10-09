-- Haskell 2010 Report 19.1 (Data.Ix): Ix at tuples of size 2, 3, 4 and 5 (row-major order: last component
-- varies fastest)
module Main (main) where

import Data.Ix

b2 :: ((Int, Int), (Int, Int))
b2 = ((1, 1), (3, 4))

b3 :: ((Int, Char, Bool), (Int, Char, Bool))
b3 = ((0, 'a', False), (1, 'b', True))

b4 :: ((Int, Int, Int, Int), (Int, Int, Int, Int))
b4 = ((0, 0, 0, 0), (1, 2, 1, 2))

b5 :: ((Bool, Int, Char, Ordering, Integer), (Bool, Int, Char, Ordering, Integer))
b5 = ((False, 1, 'x', LT, 10), (True, 2, 'y', GT, 11))

main :: IO ()
main = do
  print ("range pair", range ((0, 0), (1, 2) :: (Int, Int)))
  print ("range pair offset", range b2)
  print ("index pair", index b2 (1, 1), index b2 (1, 4), index b2 (2, 1), index b2 (2, 3), index b2 (3, 4))
  print ("inRange pair", inRange b2 (2, 2), inRange b2 (0, 2), inRange b2 (2, 5), inRange b2 (4, 4))
  print ("inRange is componentwise not lexicographic", inRange b2 (2, 9), (2, 9) >= fst b2 && (2, 9) <= snd b2)
  print ("rangeSize pair", rangeSize b2)
  print ("pair mixed types", range (('a', False), ('b', True)))
  print ("pair of pairs", range (((0, 0), 'a'), ((0, 1), 'b') :: ((Int, Int), Char)))
  print ("range triple", range b3)
  print ("index triple", index b3 (0, 'a', False), index b3 (0, 'b', True), index b3 (1, 'a', False), index b3 (1, 'b', True))
  print ("inRange triple", inRange b3 (1, 'a', True), inRange b3 (2, 'a', True), inRange b3 (0, 'c', False))
  print ("rangeSize triple", rangeSize b3)
  print ("range 4-tuple prefix", take 7 (range b4))
  print ("range 4-tuple last", last (range b4))
  print ("index 4-tuple", index b4 (0, 0, 0, 0), index b4 (0, 0, 1, 0), index b4 (0, 1, 0, 0), index b4 (1, 0, 0, 0), index b4 (1, 2, 1, 2))
  print ("inRange 4-tuple", inRange b4 (1, 1, 1, 1), inRange b4 (1, 1, 2, 1))
  print ("rangeSize 4-tuple", rangeSize b4)
  print ("range 5-tuple prefix", take 3 (range b5))
  print ("index 5-tuple", index b5 (False, 1, 'x', LT, 10), index b5 (False, 1, 'x', EQ, 10), index b5 (True, 2, 'y', GT, 11))
  print ("inRange 5-tuple", inRange b5 (True, 1, 'y', EQ, 11), inRange b5 (True, 1, 'z', EQ, 11))
  print ("rangeSize 5-tuple", rangeSize b5)
  print ("index enumerates range pair", map (index b2) (range b2))
  print ("index enumerates range triple", map (index b3) (range b3) == [0 .. rangeSize b3 - 1])
  print ("index enumerates range 4-tuple", map (index b4) (range b4) == [0 .. rangeSize b4 - 1])
  print ("index enumerates range 5-tuple", map (index b5) (range b5) == [0 .. rangeSize b5 - 1])
  print ("range is sorted", and (zipWith (<) (range b4) (tail (range b4))))
