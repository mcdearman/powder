-- Haskell 2010 Report 4.2.1: constructors are functions with the declared types (partial application, higher-order use)
module Main (main) where

data Shape = Circle Int | Rect Int Int deriving (Eq, Show)

data Pair a b = Pair a b deriving (Eq, Show)

data Wrap a = Wrap a deriving (Eq, Show)

data Rec = Rec { rx :: Int, ry :: Char } deriving (Eq, Show)

mkCircle :: Int -> Shape
mkCircle = Circle

mkSquareish :: Int -> Int -> Shape
mkSquareish = Rect

unitWide :: Int -> Shape
unitWide = Rect 1

pairWith :: a -> b -> Pair a b
pairWith = Pair

unwrapStr :: Wrap String -> String
unwrapStr (Wrap s) = s

apply2 :: (a -> b -> c) -> a -> b -> c
apply2 f x y = f x y

main :: IO ()
main = do
  print ("map constructor", map Circle [1, 2, 3])
  print ("zipWith constructor", zipWith Rect [1, 2] [3, 4])
  print ("partial application", map (Rect 1) [5, 6])
  print ("named", mkCircle 4, mkSquareish 2 3, unitWide 9)
  print ("polymorphic", pairWith 'a' True, pairWith (1 :: Int) "s")
  print ("flip", flip Pair 'a' True)
  print ("compose", (Wrap . Wrap) 'x')
  print ("iterate-free nesting", Wrap (Wrap (Wrap (0 :: Int))))
  print ("higher-order", apply2 Pair (1 :: Int) (2 :: Int))
  print ("foldr with constructor", foldr (\c w -> Wrap (c : unwrapStr w)) (Wrap "") "abc")
  print ("record constructor as function", zipWith Rec [1, 2] "ab")
  print ("curry-like", uncurry Rect (2, 3))
  print ("just and constructor", fmap Circle (Just 1))
  print ("equality of results", map Circle [1, 2] == [mkCircle 1, mkCircle 2])
