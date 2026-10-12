{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StandaloneDeriving, DerivingStrategies, GeneralisedNewtypeDeriving, DeriveAnyClass, DeriveFunctor #-}
-- StandaloneDeriving, DerivingStrategies, GeneralisedNewtypeDeriving and DeriveAnyClass: an instance derived apart from
-- its type, and how an instance is derived said: as the Report does, as the newtype's is, or with no methods
module Main (main) where

data Colour = Red | Green | Blue

deriving instance Eq Colour
deriving instance Ord Colour
deriving instance Show Colour
deriving instance Enum Colour
deriving instance Bounded Colour

data Box a = Box a | Empty

deriving instance Show a => Show (Box a)
deriving instance Eq a => Eq (Box a)
deriving instance Functor Box

class Describe a where
  describe :: a -> String
  describe _ = "something"
  name :: a -> String
  name _ = "nameless"

data Thing = Thing
  deriving stock (Show, Eq)
  deriving anyclass (Describe)

data Other = Other deriving (Show)

deriving anyclass instance Describe Other

-- A newtype has the instances of what it is a newtype of.
newtype Age = Age Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)

newtype Score = Score Double deriving newtype (Show, Eq, Ord, Num, Fractional)

newtype Name' = Name' String
  deriving stock (Eq)
  deriving newtype (Semigroup, Monoid, Show)

newtype Count = Count Integer
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num)

newtype Wrapped a = Wrapped [a]
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid, Eq)

newtype Meters = Meters Int

deriving stock instance Show Meters
deriving newtype instance Eq Meters
deriving newtype instance Num Meters

-- A class of type constructors, for a newtype of something applied to its last parameter.
newtype Opt a = Opt (Maybe a)
  deriving stock (Show)
  deriving newtype (Functor, Applicative, Monad)

class Pretty a where
  pretty :: a -> String

instance Pretty Int where
  pretty n = "<" ++ show n ++ ">"

newtype Id' = Id' Int deriving newtype (Pretty)

main :: IO ()
main = do
  print ([minBound .. maxBound :: Colour], Red < Blue, succ Red, Green == Green)
  print (Box 'b', Empty :: Box Int, Box 1 == Box (1 :: Int), fmap (+ 1) (Box (1 :: Int)))
  putStrLn (describe Thing ++ ", " ++ name Thing ++ ", " ++ describe Other)
  print (Thing == Thing)
  print (Age 30 + 1, Age 3 * Age 4, Age 1 < Age 2, [Age 1 .. Age 3], toInteger (Age 7 `div` 2), fromIntegral (Age 5) + (0.5 :: Double))
  print (Score 1.5 + 2, Score 3 / 2, Score 1 == Score 1)
  print (Name' "ab" <> Name' "cd", mempty :: Name', Name' "x" == Name' "x")
  print (Count 2 + 3, Count 5 * 5 - 1, sum [Count 1, Count 2, Count 3])
  print (Wrapped "ab" <> Wrapped "cd", mconcat [Wrapped [1 :: Int], Wrapped [2]], Wrapped "x" == Wrapped "x")
  print (Meters 3 + Meters 4, Meters 1 == Meters 1)
  print (fmap (+ 1) (Opt (Just (1 :: Int))), pure 'p' :: Opt Char, Opt (Just 3) >>= \x -> Opt (if x > (2 :: Int) then Just (x * 2) else Nothing))
  print (do { a <- Opt (Just 1); b <- Opt Nothing; return (a + b :: Int) })
  putStrLn (pretty (Id' 5))
