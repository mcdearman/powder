{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances, ConstraintKinds, UndecidableInstances #-}
-- FlexibleInstances and TypeSynonymInstances: an instance at any type, and at a synonym; the pragmas that say which
-- of two instances that fit is taken; and ConstraintKinds: a name for some constraints
module Main (main) where

class Describe a where
  describe :: a -> String

-- At types that are more than a constructor applied to variables.
instance Describe [Char] where
  describe s = "text " ++ s

instance Describe [Int] where
  describe xs = "numbers " ++ show (sum xs)

instance Describe (Maybe Int) where
  describe = maybe "no number" (("number " ++) . show)

instance Describe (Maybe Bool) where
  describe = maybe "no flag" (("flag " ++) . show)

instance Describe (Either a Int) where
  describe = either (const "left") (("right " ++) . show)

instance Describe (a, a) where
  describe _ = "a pair of one type"

instance Describe (Int -> Int) where
  describe f = "a function giving " ++ show (f 0)

type Name = String

type Point = (Double, Double)

class Pretty a where
  pretty :: a -> String

-- At a synonym, which is at what it stands for.
instance Pretty Name where
  pretty n = "<" ++ n ++ ">"

instance Pretty Point where
  pretty (x, y) = show x ++ "," ++ show y

-- Two that fit: the more particular is taken, where they say it may be.
class Render a where
  render :: a -> String

instance {-# OVERLAPPABLE #-} Show a => Render a where
  render = show

instance {-# OVERLAPPING #-} Render String where
  render s = s

instance Render Bool where
  render b = if b then "yes" else "no"

instance {-# OVERLAPPING #-} Render a => Render [a] where
  render xs = "[" ++ concatMap ((++ ";") . render) xs ++ "]"

class Size a where
  size :: a -> Int

instance {-# OVERLAPS #-} Size [a] where
  size = length

instance {-# OVERLAPS #-} Size [Char] where
  size s = length (words s)

-- A name for constraints, with a parameter, and by way of another.
type Stringy a = (Show a, Read a)

type Numeric a = (Num a, Ord a)

type Both a = (Stringy a, Numeric a)

roundTrip :: Stringy a => a -> a
roundTrip = read . show

clampTo :: Numeric a => a -> a -> a
clampTo limit x = if x > limit then limit else x + 0

bump :: Both a => a -> a
bump x = clampTo 100 (roundTrip x + 1)

main :: IO ()
main = do
  putStrLn (describe "hello")
  putStrLn (describe [1, 2, 3 :: Int])
  putStrLn (describe (Just (5 :: Int)) ++ ", " ++ describe (Just True) ++ ", " ++ describe (Nothing :: Maybe Bool))
  putStrLn (describe (Right 3 :: Either Char Int) ++ ", " ++ describe ('a', 'b') ++ ", " ++ describe ((+ 1) :: Int -> Int))
  putStrLn (pretty ("ann" :: Name) ++ " " ++ pretty ((1, 2.5) :: Point))
  putStrLn (render (5 :: Int) ++ " " ++ render "text" ++ " " ++ render True ++ " " ++ render 'c')
  putStrLn (render [1, 2 :: Int] ++ " " ++ render ["a", "b"] ++ " " ++ render [[True], []])
  print (size [1, 2, 3 :: Int], size "three little words")
  print (roundTrip (5 :: Int), roundTrip [True, False], clampTo 10 (15 :: Int), bump (5 :: Int), bump (200 :: Double))
