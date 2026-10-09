-- Haskell 2010 Report 4.3.4: default (Rational): any type that is an instance of Num may be listed
module Main (main) where

default (Rational)

main :: IO ()
main = do
  print ("third", 1 / 3)
  print ("sum of fractions", 1 / 3 + 1 / 6)
  print ("literal", 3)
  print ("decimal literal", 0.75)
  print ("exact decimal arithmetic", 0.1 + 0.2 == 0.3)
  print ("list", [1, 0.5])
  print ("power", (1 / 2) ^ (3 :: Int))
  print ("truncate", truncate (7 / 2) :: Int)
  print ("negative", negate (2 / 4))
  print ("show", show (4 / 6))
