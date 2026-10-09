-- Haskell 2010 Report 9: class Bounded (minBound, maxBound) at Bool, Char, Int, Ordering, () and tuples
module Main (main) where

boundsOf :: Bounded a => a -> (a, a)
boundsOf _ = (minBound, maxBound)

main :: IO ()
main = do
  print ("Bool", minBound :: Bool, maxBound :: Bool)
  print ("Ordering", minBound :: Ordering, maxBound :: Ordering)
  print ("unit", minBound :: (), maxBound :: ())
  print ("Int", minBound :: Int, maxBound :: Int)
  print ("Int values", (minBound :: Int) == -9223372036854775808, (maxBound :: Int) == 9223372036854775807)
  print ("Int wrap", (maxBound :: Int) + 1 == minBound, (minBound :: Int) - 1 == maxBound)
  print ("Char", minBound :: Char, fromEnum (maxBound :: Char), fromEnum (minBound :: Char))
  print ("pair", minBound :: (Bool, Ordering), maxBound :: (Bool, Ordering))
  print ("triple", minBound :: (Bool, (), Ordering), maxBound :: (Bool, (), Ordering))
  print ("nested", maxBound :: ((Bool, Bool), Ordering), minBound :: (Int, Bool))
  print ("quad", maxBound :: (Bool, Bool, Bool, Bool))
  print ("polymorphic", boundsOf True, boundsOf EQ, boundsOf (0 :: Int))
  print ("with enum", [minBound .. maxBound :: Bool], [minBound .. maxBound :: Ordering])
  print ("range sizes", length [minBound .. maxBound :: Char], fromEnum (maxBound :: Bool) - fromEnum (minBound :: Bool))
