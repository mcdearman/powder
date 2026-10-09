-- Haskell 2010 Report 3.2: an identifier in backquotes is an infix operator (default fixity infixl 9)
module Main (main) where

data V = V Int Int deriving (Eq, Show)

plus3 :: Int -> Int -> Int -> Int
plus3 a b c = a + b + c

sub :: Int -> Int -> Int
x `sub` y = x - y

mul :: Int -> Int -> Int
mul a b = a * b

main :: IO ()
main = do
  print ("div/mod", 7 `div` 2, 7 `mod` 2)
  print ("quot/rem", 7 `quot` 2, 7 `rem` 2)
  print ("elem", 3 `elem` [1, 2, 3], 'z' `notElem` "abc")
  print ("max chain", 1 `max` 2 `max` 3)
  print ("subtract", 10 `subtract` 3)
  print ("constructor", 2 `V` 3, 2 `V` 3 == V 2 3)
  print ("three args", (1 `plus3` 2) 3)
  print ("defined infix", 10 `sub` 3, sub 10 3)
  print ("left assoc default", 10 `sub` 3 `sub` 2)
  print ("precedence 9 default", 1 + 2 `mul` 3, 2 `mul` 3 + 1)
  print ("tighter than ^", 2 ^ 1 `mul` 3)
  print ("seq/const", 3 `seq` 4, 5 `const` 6)
  print ("pattern", case V 1 2 of { a `V` b -> a + b })
  print ("local", let a `f` b = a * 2 + b in 3 `f` 4)
  print ("same as prefix", 7 `divMod` 2 == divMod 7 2)
