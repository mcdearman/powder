-- Haskell 2010 Report 11: a derived instance for a parameterised type gets the smallest context that makes the
-- field instances available (`deriving Eq` on `data T a` gives `instance Eq a => Eq (T a)`)
module Main (main) where

data Opt a = None | Some a deriving (Eq, Ord, Show, Read)

data Pair a b = Pair a b deriving (Eq, Ord, Show, Read, Bounded)

-- the parameter occurs only inside other type constructors
data Wrap a = Wrap [a] (Maybe a) (a, Int) deriving (Eq, Ord, Show, Read)

-- the parameter does not occur in any field: no constraint is needed on it
data Phantom a = Phantom Int deriving (Eq, Ord, Show)

-- only the first parameter is used
data First a b = First a | Neither deriving (Eq, Ord, Show)

same :: Eq a => Opt a -> Opt a -> Bool
same x y = x == y

bigger :: (Ord a, Ord b) => Pair a b -> Pair a b -> Pair a b
bigger = max

render :: Show a => Wrap a -> String
render w = showsPrec 11 w ""

parse :: Read a => String -> Opt a
parse = read

lowest :: (Bounded a, Bounded b) => Pair a b
lowest = minBound

main :: IO ()
main = do
  print ("Eq a => Eq (Opt a)", same (Some 'x') (Some 'x'), same (Some 'x') None, same (Some [1 :: Int]) (Some [2]))
  print ("nested instantiation", Some (Some (1 :: Int)) == Some (Some 1), Some (Some 'a') < Some None, Some (Pair 'a' True) == Some (Pair 'a' True))
  print ("Ord a, Ord b", bigger (Pair 1 'a') (Pair (1 :: Int) 'b'), bigger (Pair "z" ()) (Pair "a" ()))
  print ("Show a", render (Wrap [1 :: Int] (Just 2) (3, 4)), render (Wrap "ab" Nothing ('c', 0)))
  print ("Read a", parse "Some 3" :: Opt Int, parse "Some (Some True)" :: Opt (Opt Bool), parse "None" :: Opt Char)
  print ("Bounded a, Bounded b", lowest :: Pair Bool Ordering, maxBound :: Pair Char Bool, minBound :: Pair () (Pair Bool Bool))
  print ("Wrap", Wrap [1 :: Int] Nothing (1, 0) < Wrap [1] (Just 0) (0, 0), read "Wrap [1.5] Nothing (2.5,1)" == Wrap [1.5 :: Double] Nothing (2.5, 1))
  print ("phantom parameter needs no instance", (Phantom 1 :: Phantom (Int -> Int)) == Phantom 1, compare (Phantom 1 :: Phantom (IO ())) (Phantom 2))
  print ("phantom show", Phantom 7 :: Phantom (Int -> Int))
  print ("unused parameter needs no instance", (First 'a' :: First Char (Int -> Int)) == First 'a', (Neither :: First Char (IO ())) > First 'z')
  print ("unused parameter show", [First 1, Neither :: First Int (Int -> Int)])
  print ("in containers", [None, Some 1] == [None, Some (1 :: Int)], maximum [Some 'a', None, Some 'c'], lookup (Some 2) [(None, 'n'), (Some (2 :: Int), 's')])
