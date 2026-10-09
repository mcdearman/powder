-- Haskell 2010 Report 4.3.1: superclasses; superclass methods are available from a subclass constraint
module Main (main) where

class Shape a where
  area :: a -> Int
  name :: a -> String

class Shape a => Solid a where
  height :: a -> Int
  volume :: a -> Int
  volume x = area x * height x

class Solid a => Heavy a where
  density :: a -> Int

data Box = Box Int Int Int

data Slab = Slab Int

instance Shape Box where
  area (Box w d _) = w * d
  name _ = "box"

instance Solid Box where
  height (Box _ _ h) = h

instance Heavy Box where
  density _ = 3

instance Shape Slab where
  area (Slab s) = s * s
  name _ = "slab"

instance Solid Slab where
  height _ = 1
  volume s = area s

-- only the subclass is mentioned in the context
describeSolid :: Solid a => a -> String
describeSolid s = name s ++ ":" ++ show (area s) ++ ":" ++ show (volume s)

-- two levels up the hierarchy
mass :: Heavy a => a -> Int
mass x = density x * volume x + area x

-- Prelude hierarchies: Eq from Ord, Num and Ord from Integral, Fractional from RealFrac
sortedPair :: Ord a => a -> a -> Bool
sortedPair x y = x == y || x < y

collatzStep :: Integral a => a -> a
collatzStep n = if n == 1 then 1 else if even n then n `div` 2 else 3 * n + 1

halfIfSmall :: RealFrac a => a -> a
halfIfSmall x = if x < 10 then x / 2 else fromIntegral (truncate x :: Int)

main :: IO ()
main = do
  print ("describe box", describeSolid (Box 2 3 4))
  print ("describe slab", describeSolid (Slab 5))
  print ("mass", mass (Box 2 3 4))
  print ("volume default", volume (Box 1 1 9))
  print ("volume overridden", volume (Slab 3))
  print ("sortedPair", sortedPair 'a' 'b', sortedPair 'b' 'a', sortedPair "x" "x")
  print ("collatz", map collatzStep [1, 6, 7 :: Int])
  print ("collatz integer", collatzStep (27 :: Integer))
  print ("halfIfSmall", halfIfSmall (3 :: Double), halfIfSmall (12.75 :: Double))
