{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DefaultSignatures, DeriveAnyClass, DerivingStrategies #-}
-- DefaultSignatures: a method's default may be of a type of its own, which asks more than the method's does; an
-- instance that does not define the method must have what the default asks
module Main (main) where

import Data.List (intercalate)

class Describe a where
  describe :: a -> String
  default describe :: Show a => a -> String
  describe x = "<" ++ show x ++ ">"

  -- A default as the Report has one, beside the other.
  name :: a -> String
  name _ = "thing"

  both :: a -> String
  both x = name x ++ " " ++ describe x

data Colour = Red | Green deriving (Show)

-- With nothing said, the default: which asks that it can be shown.
instance Describe Colour

instance Describe Int where
  name _ = "number"

-- With the method given, nothing is asked: this type cannot be shown.
newtype Secret = Secret Int

instance Describe Secret where
  describe _ = "<hidden>"
  name _ = "secret"

-- What the default asks of a type with a parameter is asked by the instance.
instance Show a => Describe [a]

instance (Show a, Show b) => Describe (a, b) where
  name _ = "pair"

-- An instance with no methods, derived.
data Shape = Circle Int | Square Int
  deriving (Show)
  deriving anyclass (Describe)

-- A default that needs two classes, and uses the class's other methods.
class Container f where
  elements :: f a -> [a]

  summary :: Show a => f a -> String
  default summary :: (Show a, Functor f) => f a -> String
  summary xs = intercalate "," (elements (fmap show xs))

  size :: f a -> Int
  default size :: Foldable f => f a -> Int
  size = length

newtype Box a = Box [a] deriving (Functor, Foldable)

instance Container Box where
  elements (Box xs) = xs

instance Container Maybe where
  elements = maybe [] pure

-- Not a functor: so it says how itself.
newtype Evens a = Evens [a]

instance Container Evens where
  elements (Evens xs) = [x | (i, x) <- zip [0 :: Int ..] xs, even i]
  summary xs = unwords (map show (elements xs))
  size = length . elements

-- A default of the method's own type, said again.
class Twice a where
  twice :: a -> a
  default twice :: a -> a
  twice x = x

instance Twice Bool

instance Twice Int where
  twice = (* 2)

main :: IO ()
main = do
  putStrLn (both Red)
  putStrLn (both (7 :: Int))
  putStrLn (both (Secret 7))
  putStrLn (both [Red, Green])
  putStrLn (both (Green, 'x'))
  putStrLn (both (Circle 3))
  putStrLn (summary (Box [1, 2, 3 :: Int]) ++ " " ++ show (size (Box "abcd")))
  putStrLn (summary (Just 'x') ++ " " ++ show (size (Nothing :: Maybe Int)))
  putStrLn (summary (Evens "abcde") ++ " " ++ show (size (Evens "abcde")))
  print (twice True, twice (21 :: Int))
