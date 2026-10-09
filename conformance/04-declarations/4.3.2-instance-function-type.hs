-- Haskell 2010 Report 4.3.2: instances for the function type, written (a -> b) and ((->) a b)
module Main (main) where

class Finite a where
  universe :: [a]

instance Finite Bool where
  universe = [False, True]

instance Finite () where
  universe = [()]

instance (Finite a, Finite b) => Finite (a, b) where
  universe = [(x, y) | x <- universe, y <- universe]

-- a property holds if it holds for every argument
class Testable a where
  test :: a -> Bool

instance Testable Bool where
  test b = b

instance (Finite a, Testable b) => Testable (a -> b) where
  test f = all (test . f) universe

class Describe a where
  describe :: a -> String

instance Describe Bool where
  describe b = if b then "yes" else "no"

instance Describe ((->) a b) where
  describe _ = "function"

-- functions compared extensionally through a user class
class Same a where
  same :: a -> a -> Bool

instance Same Bool where
  same x y = x == y

instance (Finite a, Same b) => Same (a -> b) where
  same f g = all (\x -> same (f x) (g x)) universe

main :: IO ()
main = do
  print ("constant", test True, test False)
  print ("excluded middle", test (\x -> x || not x))
  print ("identity is not valid", test (id :: Bool -> Bool))
  print ("commutative and", test (\x y -> (x && y) == (y && x)))
  print ("de morgan", test (\x y -> not (x && y) == (not x || not y)))
  print ("three arguments", test (\x y z -> ((x && y) && z) == (x && (y && z))))
  print ("implication fails", test (\x y -> not x || y))
  print ("unit argument", test (\() -> True))
  print ("pair argument", test (\(x, y) -> x || y || not x))
  print ("named function", test (||), test not)
  print ("describe", describe True, describe not, describe (&&))
  print ("same", same not not, same not id, same (&&) (\x y -> y && x))
  print ("same differs", same (&&) (||))
