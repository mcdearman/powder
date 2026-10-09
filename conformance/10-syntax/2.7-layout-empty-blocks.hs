-- Haskell 2010 Report 10.3: empty implicit blocks: where/let/of followed by a token at an indentation <= the enclosing block
module Main (main) where

class Marker a where

instance Marker Int where

instance Marker Bool

data Unit2 = Unit2 deriving Show

f :: Int -> Int
f x = x + 1
  where

g :: Int -> Int
g x = let in x * 2

h :: Int -> Int
h x = let {} in x - 1

isMarked :: Marker a => a -> String
isMarked _ = "marked"

main :: IO ()
main = do
  print ("f", f 1)
  print ("g", g 2)
  print ("h", h 3)
  print ("marker", isMarked (1 :: Int), isMarked True)
  let
  print ("empty let statement", Unit2)
  print ("empty where braces", k)
  where { }

k :: Int
k = 9 where
