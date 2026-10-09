-- Haskell 2010 Report 4.4.1: a signature may give a type less general than the principal type
module Main (main) where

-- principal type: a -> a
idInt :: Int -> Int
idInt x = x

-- principal type: a -> b -> (a, b)
samePair :: a -> a -> (a, a)
samePair x y = (x, y)

-- principal type: Eq a => a -> a -> Bool
ordEq :: Ord a => a -> a -> Bool
ordEq x y = x == y

-- principal type: a -> a (the context is not needed)
showable :: Show a => a -> a
showable x = x

-- principal type: Num a => a -> a
doubleInt :: Int -> Int
doubleInt x = x + x

-- principal type: [a] -> Int
countChars :: String -> Int
countChars = length

-- principal type: (a -> b) -> [a] -> [b]
mapInts :: (Int -> Int) -> [Int] -> [Int]
mapInts f xs = map f xs

-- principal type: a
emptyBools :: [Bool]
emptyBools = []

-- principal type: Functor f => f a -> f a
maybeOnly :: Maybe a -> Maybe a
maybeOnly = fmap id

-- principal type: (b -> c) -> (a -> b) -> a -> c, with all three made equal
composeSame :: (a -> a) -> (a -> a) -> a -> a
composeSame f g = f . g

main :: IO ()
main = do
  print ("idInt", idInt 5)
  print ("samePair", samePair 'a' 'b', samePair True False)
  print ("ordEq", ordEq 'a' 'a', ordEq [1 :: Int] [2])
  print ("showable", showable 'x', showable (1 :: Int))
  print ("doubleInt", doubleInt 21)
  print ("countChars", countChars "hello")
  print ("mapInts", mapInts (+ 1) [1, 2, 3])
  print ("emptyBools", emptyBools, True : emptyBools)
  print ("maybeOnly", maybeOnly (Just 'j'), maybeOnly (Nothing :: Maybe Int))
  print ("composeSame", composeSame (+ 1) (* 2) (5 :: Int), composeSame not not True)
