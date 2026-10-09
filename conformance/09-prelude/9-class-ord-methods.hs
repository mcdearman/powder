-- Haskell 2010 Report 9: class Ord, compare (<) (<=) (>=) (>) max min at the Prelude types, and the default methods
module Main (main) where

import Data.Ratio ((%))

-- only compare defined
data ByLen = ByLen String

instance Eq ByLen where
  ByLen a == ByLen b = length a == length b

instance Ord ByLen where
  compare (ByLen a) (ByLen b) = compare (length a) (length b)

-- only (<=) defined
data Rev = Rev Int

instance Eq Rev where
  Rev a == Rev b = a == b

instance Ord Rev where
  Rev a <= Rev b = b <= a

unByLen :: ByLen -> String
unByLen (ByLen s) = s

unRev :: Rev -> Int
unRev (Rev n) = n

allOps :: Ord a => a -> a -> (Ordering, Bool, Bool, Bool, Bool)
allOps x y = (compare x y, x < y, x <= y, x >= y, x > y)

main :: IO ()
main = do
  print ("Int lt", allOps (1 :: Int) 2)
  print ("Int eq", allOps (2 :: Int) 2)
  print ("Int gt", allOps (3 :: Int) 2)
  print ("Int max/min", max (3 :: Int) 7, min (3 :: Int) 7, max (-1 :: Int) (-2), min (-1 :: Int) (-2))
  print ("Integer", allOps (10 ^ (30 :: Int) :: Integer) (10 ^ (29 :: Int)), max (-5 :: Integer) 5)
  print ("Char", allOps 'a' 'b', max 'a' 'Z', min 'a' 'Z')
  print ("Bool", allOps False True, max False True)
  print ("Ordering", allOps GT EQ, min LT GT)
  print ("unit", allOps () ())
  print ("Double", allOps (1.5 :: Double) 2.5, max (0.5 :: Double) (-0.5), min (1.0e10 :: Double) 1.0e-10)
  print ("Float", allOps (2.5 :: Float) 2.5, max (1.5 :: Float) 2.5)
  print ("Rational", allOps (1 % 3 :: Rational) (1 % 2), max (1 % 3 :: Rational) (1 % 2))
  print ("String", allOps "abc" "abd", allOps "ab" "abc")
  print ("String more", compare "" "", compare "" "a", compare "b" "abc", max "apple" "apricot")
  print ("list", allOps [1, 2, 3 :: Int] [1, 3], compare [1, 2 :: Int] [1, 2], min [2 :: Int] [1, 9, 9])
  print ("Maybe", allOps Nothing (Just 'a'), compare (Just 'b') (Just 'a'), max (Just 1) (Nothing :: Maybe Int))
  print ("Either", allOps (Left 5 :: Either Int Int) (Right 1), compare (Right 1 :: Either Int Int) (Right 2))
  print ("pair", allOps (1 :: Int, 'b') (1, 'a'), compare (1 :: Int, 'z') (2, 'a'))
  print ("triple", compare (1 :: Int, 'a', False) (1, 'a', True), max (1 :: Int, 2 :: Int, 3 :: Int) (1, 3, 0))
  print ("from compare", allOps (ByLen "aa") (ByLen "b"))
  print ("from compare max/min", unByLen (max (ByLen "aa") (ByLen "b")), unByLen (min (ByLen "aa") (ByLen "b")))
  print ("from <=", allOps (Rev 1) (Rev 2), allOps (Rev 2) (Rev 2))
  print ("from <= max/min", unRev (max (Rev 1) (Rev 2)), unRev (min (Rev 1) (Rev 2)))
