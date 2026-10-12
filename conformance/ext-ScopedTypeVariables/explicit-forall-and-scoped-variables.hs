{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, InstanceSigs, ConstrainedClassMethods #-}
-- ExplicitForAll, ScopedTypeVariables, InstanceSigs and ConstrainedClassMethods: a signature may name its type
-- variables, which are then in scope in the definition; a pattern may be said to have a type; an instance may say a
-- method's type again; and a method's context may ask more of the class's variable
module Main (main) where

import Data.List (sortBy)

identity :: forall a. a -> a
identity x = x

pairUp :: forall a b. a -> b -> (a, b)
pairUp x y = (x, y)

-- The variables a forall names are in scope in the definition.
sortOn' :: forall a b. Ord b => (a -> b) -> [a] -> [a]
sortOn' f = map snd . sortBy cmp . map keyed
  where
    keyed :: a -> (b, a)
    keyed x = (f x, x)
    cmp :: (b, a) -> (b, a) -> Ordering
    cmp (k, _) (k', _) = compare k k'

reverseBoth :: forall a. [a] -> ([a], [a])
reverseBoth xs = (ys, zs)
  where
    ys :: [a]
    ys = reverse xs
    zs = (reverse ys :: [a])

minMax :: forall a. (Ord a, Bounded a) => [a] -> (a, a)
minMax = foldr step (maxBound, minBound)
  where
    step :: a -> (a, a) -> (a, a)
    step x (lo, hi) = (min x lo, max x hi)

-- Without a forall they are not, and a local signature's variables are its own.
general :: a -> [a]
general x = local' x
  where
    local' :: b -> [b]
    local' y = [y, y]

-- A pattern said to have a type.
total :: [Int] -> Int
total xs = go xs 0
  where
    go [] (acc :: Int) = acc
    go ((y :: Int) : ys) acc = go ys (acc + y)

describe :: Maybe Int -> String
describe (Just (n :: Int)) = show n
describe Nothing = "nothing"

class Container f where
  empty :: f a
  insert :: a -> f a -> f a
  -- A method's context may ask more of the class's variable than the class does.
  size :: Foldable f => f a -> Int
  size = length
  toL :: f a -> [a]

newtype Stack a = Stack [a]

instance Container Stack where
  empty :: Stack a
  empty = Stack []
  insert :: a -> Stack a -> Stack a
  insert x (Stack xs) = Stack (x : xs)
  size :: Stack a -> Int
  size (Stack xs) = length xs
  toL :: forall a. Stack a -> [a]
  toL (Stack xs) = result
    where
      result :: [a]
      result = xs

instance Container [] where
  empty = []
  insert = (:)
  toL = id

class Shape a where
  area :: a -> Double
  -- More is asked of the class's variable by one method.
  bigger :: Ord a => a -> a -> a
  bigger x y = if x > y then x else y

newtype Square = Square Double deriving (Eq, Ord, Show)

instance Shape Square where
  area (Square s) = s * s

-- An instance's variables are in scope in its methods.
newtype Box a = Box a

instance forall a. Show a => Show (Box a) where
  show (Box x) = "Box<" ++ shown ++ ">"
    where
      shown = show (x :: a)

main :: IO ()
main = do
  print (identity 'x', pairUp (1 :: Int) "two")
  print (sortOn' negate [3, 1, 2 :: Int], sortOn' snd [(1 :: Int, 'b'), (2, 'a')])
  print (reverseBoth "abc")
  print (minMax [3, 1, 2 :: Int], minMax "hello")
  print (general True)
  print (total [1, 2, 3], describe (Just 5), describe Nothing)
  print (toL (insert 1 (insert 2 empty) :: Stack Int))
  print (toL (insert 1 (insert (2 :: Int) empty) :: [Int]), size (insert 'a' (empty :: [Char])))
  print (area (Square 3), bigger (Square 1) (Square 2))
  print (Box (5 :: Int), Box "s")
  print ((\(x :: Int) (y :: Int) -> x + y) 1 2)
  print (case (3 :: Int, 'c') of (n :: Int, c :: Char) -> (c, n))
