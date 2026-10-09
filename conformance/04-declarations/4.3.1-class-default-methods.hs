-- Haskell 2010 Report 4.3.1: class declarations with default class methods, used or overridden by instances
module Main (main) where

class Animal a where
  name :: a -> String
  sound :: a -> String
  sound _ = "..."
  speak :: a -> String
  speak x = name x ++ " says " ++ sound x
  legs :: a -> Int
  legs _ = 4

data Dog = Dog

data Fish = Fish

data Bird = Bird

instance Animal Dog where
  name _ = "dog"
  sound _ = "woof"

instance Animal Fish where
  name _ = "fish"
  legs _ = 0

instance Animal Bird where
  name _ = "bird"
  sound _ = "tweet"
  legs _ = 2
  speak x = "the " ++ name x ++ " sings " ++ sound x

-- defaults written with guards, where and several clauses
class Scale a where
  magnitude :: a -> Int
  grade :: a -> String
  grade x
    | m >= limit = "high"
    | m > 0 = "low"
    | otherwise = "none"
    where
      m = magnitude x
      limit = 10
  isZero :: a -> Bool
  isZero x = magnitude x == 0

instance Scale Int where
  magnitude = abs

instance Scale Bool where
  magnitude b = if b then 1 else 0
  isZero = not

instance Scale a => Scale [a] where
  magnitude = sum . map magnitude

summary :: Animal a => a -> (String, Int)
summary x = (speak x, legs x)

main :: IO ()
main = do
  print ("dog", summary Dog)
  print ("fish", summary Fish)
  print ("bird", summary Bird)
  print ("sound default", sound Fish)
  print ("grade int", map grade [0, 5, -20 :: Int])
  print ("grade bool", grade True, grade False)
  print ("grade list", grade [3, 4, 5 :: Int], grade ([] :: [Bool]))
  print ("isZero default", isZero (0 :: Int), isZero [0, 0 :: Int])
  print ("isZero overridden", isZero True, isZero False)
