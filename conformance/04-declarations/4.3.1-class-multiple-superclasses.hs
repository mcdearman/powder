-- Haskell 2010 Report 4.3.1: classes with several superclasses, and a diamond-shaped hierarchy
module Main (main) where

class (Eq a, Show a) => Entity a where
  ident :: a -> Int

class (Ord a, Bounded a, Enum a) => Finite a where
  universe :: [a]
  universe = [minBound .. maxBound]

class Show a => Loud a where
  loud :: a -> String
  loud x = show x ++ "!"

class Show a => Quiet a where
  quiet :: a -> String
  quiet x = "(" ++ show x ++ ")"

class (Loud a, Quiet a) => Voiced a where
  voice :: Bool -> a -> String
  voice True = loud
  voice False = quiet

data Color = Red | Green | Blue deriving (Eq, Ord, Show, Enum, Bounded)

instance Entity Color where
  ident = fromEnum

instance Entity Int where
  ident n = n

instance Finite Color

instance Finite Bool

instance Loud Color

instance Quiet Color

instance Voiced Color

report :: Entity a => a -> a -> String
report x y
  | x == y = show x ++ " same as " ++ show y
  | otherwise = show (ident x) ++ " vs " ++ show (ident y)

rankOf :: Finite a => a -> Int
rankOf x = length (filter (<= x) universe)

-- Show is reachable through both superclasses
both :: Voiced a => a -> String
both x = voice True x ++ voice False x ++ show x

main :: IO ()
main = do
  print ("report same", report Red Red)
  print ("report differ", report Red Blue)
  print ("report int", report (3 :: Int) 4)
  print ("universe color", universe :: [Color])
  print ("universe bool", universe :: [Bool])
  print ("rankOf", rankOf Green, rankOf True, rankOf Blue)
  print ("voiced", both Green)
  print ("loud quiet", loud Red, quiet Blue)
