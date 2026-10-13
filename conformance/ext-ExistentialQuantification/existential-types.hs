{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ExistentialQuantification, GADTSyntax #-}
-- ExistentialQuantification: a constructor with type variables of its own, and a context: what it makes carries
-- what its context asks, which is known again where it is matched
module Main (main) where

import Data.List (sortBy)

data Showable = forall a. Show a => MkShowable a

instance Show Showable where
  showsPrec d (MkShowable a) = showsPrec d a

-- Several of its own, a context of several classes, and a field that is not of them.
data Keyed = forall k v. (Ord k, Show k, Show v) => Keyed String [(k, v)]

-- Given by its type: the variables its fields have and its result has not are its own.
data Shape where
  Shape :: (HasArea a, Show a) => a -> Shape
  Plain :: Double -> Shape

class HasArea a where
  area :: a -> Double

newtype Circle = Circle Double deriving (Show)

data Rect = Rect Double Double deriving (Show)

instance HasArea Circle where
  area (Circle r) = 3 * r * r

instance HasArea Rect where
  area (Rect w h) = w * h

-- A type's own parameter beside a constructor's own variable.
data Stream a = forall s. Stream (s -> Maybe (a, s)) s

-- No context: nothing is known of what is inside but that it fits together.
data Counter = forall s. Counter s (s -> s) (s -> Int)

-- A context and no variable of its own: the dictionary is kept all the same.
data Ordered a = Ord a => Ordered [a]

toList :: Stream a -> [a]
toList (Stream next s0) = go s0
  where
    go s = case next s of
      Nothing -> []
      Just (a, s') -> a : go s'

fromTo :: Int -> Int -> Stream Int
fromTo lo hi = Stream (\n -> if n > hi then Nothing else Just (n, n + 1)) lo

mapS :: (a -> b) -> Stream a -> Stream b
mapS f (Stream next s0) = Stream (\s -> case next s of Nothing -> Nothing; Just (a, s') -> Just (f a, s')) s0

describe :: Keyed -> String
describe (Keyed name pairs) = name ++ ": " ++ concatMap entry (sortBy (\a b -> compare (fst a) (fst b)) pairs)
  where
    entry (k, v) = show k ++ "=" ++ show v ++ ";"

shapeArea :: Shape -> Double
shapeArea (Shape s) = area s
shapeArea (Plain a) = a

shapeName :: Shape -> String
shapeName s = case s of
  Shape inner -> show inner
  Plain _ -> "plain"

run :: Int -> Counter -> Int
run 0 (Counter s _ out) = out s
run n (Counter s step out) = run (n - 1) (Counter (step s) step out)

smallest :: Ordered a -> Maybe a
smallest (Ordered []) = Nothing
smallest (Ordered xs) = Just (minimum xs)

main :: IO ()
main = do
  print [MkShowable (1 :: Int), MkShowable "two", MkShowable (Just True), MkShowable [MkShowable 'c']]
  putStrLn (describe (Keyed "numbers" [(3 :: Int, "c"), (1, "a"), (2, "b")]))
  putStrLn (describe (Keyed "letters" [('z', True), ('a', False)]))
  print (map shapeArea [Shape (Circle 1), Shape (Rect 2 3), Plain 7])
  print (map shapeName [Shape (Circle 1), Shape (Rect 2 3), Plain 7])
  print (toList (fromTo 1 5), toList (mapS (* 2) (fromTo 1 3)))
  print (run 3 (Counter (0 :: Int) (+ 2) id), run 2 (Counter "" ('x' :) length))
  print (smallest (Ordered [3, 1, 2 :: Int]), smallest (Ordered "hello"), smallest (Ordered ([] :: [Bool])))
  mapM_ (\(MkShowable x) -> putStrLn (show x)) [MkShowable (), MkShowable 2.5]
  print [show a | MkShowable a <- [MkShowable 'x', MkShowable (Left 1 :: Either Int Bool)]]
