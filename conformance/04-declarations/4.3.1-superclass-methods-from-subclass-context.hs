-- Haskell 2010 Report 4.3.1: a context naming a class gives its superclasses' methods too
module Main (main) where

check :: Ord a => a -> a -> Bool
check a b = a == b

class Show a => Labelled a where
  label :: a -> String
  label x = "<" ++ show x ++ ">"

class (Eq a, Labelled a) => Keyed a where
  same :: a -> a -> String
  same x y = if x == y then label x else label x ++ label y

instance Labelled Bool

instance Labelled Int where
  label n = "#" ++ show n

instance Keyed Bool

instance Keyed Int

describe :: Keyed a => [a] -> String
describe xs = concat [same x y | (x, y) <- zip xs (drop 1 xs)] ++ show (length (filter (== head xs) xs)) ++ show (take 1 xs)

main :: IO ()
main = do
  print (check 'a' 'a', check (1 :: Int) 2, check "x" "x", check [LT] [GT])
  putStrLn (same True True ++ same True False ++ same (1 :: Int) 1 ++ same (1 :: Int) 2)
  putStrLn (describe [True, True, False])
  putStrLn (describe [3, 4, 3 :: Int])
