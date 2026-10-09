-- Haskell 2010 Report 9: Prelude types Char, String, Int, Integer, Float, Double, Rational, IO, lists, tuples, unit, functions
module Main (main) where

import Data.Ratio ((%))

aChar :: Char
aChar = 'q'

aString :: String
aString = "hello"

charList :: [Char]
charList = aString

anInt :: Int
anInt = 9223372036854775807

anInteger :: Integer
anInteger = 123456789012345678901234567890

aFloat :: Float
aFloat = 1.5

aDouble :: Double
aDouble = 2.25

aRational :: Rational
aRational = 3 % 4

anAction :: IO Int
anAction = return 5

aFunction :: Int -> Int -> Int
aFunction x y = x * 10 + y

unitValue :: ()
unitValue = ()

triple :: (Int, Char, Bool)
triple = (1, 'x', True)

main :: IO ()
main = do
  print ("char", aChar, aChar == 'q', aChar < 'r')
  print ("string is list of char", aString == ['h', 'e', 'l', 'l', 'o'], charList, length aString)
  print ("int", anInt, anInt == maxBound, minBound :: Int)
  print ("integer", anInteger, anInteger * anInteger)
  print ("float", aFloat, aFloat + 0.25)
  print ("double", aDouble, aDouble * 2)
  print ("rational", aRational, aRational + 1 % 4)
  n <- anAction
  print ("io", n)
  print ("function", aFunction 1 2, map (aFunction 3) [4, 5])
  print ("unit", unitValue, unitValue == (), compare () (), [minBound .. maxBound :: ()])
  print ("tuple", triple, triple == (1, 'x', True), triple < (1, 'y', False))
  print ("pair", (1 :: Int, "one"), fst (1 :: Int, "one"))
  print ("list", [1, 2, 3 :: Int], [] :: [Int], 1 : 2 : [3 :: Int])
  print ("list eq/ord", [1, 2] == [1, 2 :: Int], [1, 2] < [1, 2, 0 :: Int], "abc" < "abd", "" < "a")
  print ("nested", [(1 :: Int, [Just 'a', Nothing])], ([()], ((), ())))
