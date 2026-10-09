-- Haskell 2010 Report 9: class Eq, (==) and (/=) at the Prelude types, and the default methods
module Main (main) where

import Data.Ratio ((%))

-- only (==) defined: (/=) comes from the class default
newtype Mod3 = Mod3 Int

instance Eq Mod3 where
  Mod3 a == Mod3 b = a `mod` 3 == b `mod` 3

-- only (/=) defined: (==) comes from the class default
newtype Parity = Parity Int

instance Eq Parity where
  Parity a /= Parity b = even a /= even b

main :: IO ()
main = do
  print ("Bool", True == True, True /= True, False == True)
  print ("Char", 'a' == 'a', 'a' /= 'b', 'a' == 'A')
  print ("Int", (3 :: Int) == 3, (3 :: Int) /= 3, (3 :: Int) == 4)
  print ("Integer", (2 :: Integer) ^ (70 :: Int) == 2 ^ (70 :: Int), (2 :: Integer) ^ (70 :: Int) /= 2 ^ (70 :: Int) + 1)
  print ("Float", (1.5 :: Float) == 1.5, (1.5 :: Float) /= 1.25)
  print ("Double", (0.1 + 0.2 :: Double) == 0.3, (0.5 + 0.25 :: Double) == 0.75)
  print ("Double zero", (0.0 :: Double) == (-0.0), (0 / 0 :: Double) == 0 / 0, (0 / 0 :: Double) /= 0 / 0)
  print ("Rational", (2 % 4 :: Rational) == 1 % 2, (1 % 3 :: Rational) /= 1 % 2)
  print ("Ordering", LT == LT, LT /= GT, EQ == GT)
  print ("unit", () == (), () /= ())
  print ("String", "abc" == "abc", "abc" == "abd", "" == "", "a" /= "")
  print ("list", [1, 2, 3] == [1, 2, 3 :: Int], [1, 2] == [1, 2, 3 :: Int], ([] :: [Int]) == [])
  print ("Maybe", Just 'a' == Just 'a', Just 'a' /= Nothing, (Nothing :: Maybe Char) == Nothing)
  print ("Either", (Left 1 :: Either Int Int) == Left 1, (Left 1 :: Either Int Int) /= Right 1)
  print ("pair", (1 :: Int, 'a') == (1, 'a'), (1 :: Int, 'a') == (1, 'b'))
  print ("triple", (1 :: Int, 'a', True) == (1, 'a', True), (1 :: Int, 'a', True) /= (1, 'a', False))
  print ("nested", [Just (1 :: Int, "x")] == [Just (1, "x")], [[1], [2 :: Int]] /= [[1], [3]])
  print ("default /=", Mod3 1 == Mod3 4, Mod3 1 /= Mod3 4, Mod3 1 /= Mod3 2, Mod3 1 == Mod3 2)
  print ("default ==", Parity 1 == Parity 3, Parity 1 == Parity 2, Parity 1 /= Parity 2)
  print ("in lists", [Mod3 0, Mod3 1] == [Mod3 3, Mod3 4], Mod3 5 `elem` [Mod3 0, Mod3 2])
