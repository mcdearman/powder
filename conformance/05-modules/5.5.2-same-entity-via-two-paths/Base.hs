module Base (shared, twice, Token(..), Weigh(..)) where

newtype Token = Token Int deriving (Show, Eq)

class Weigh a where
  weigh :: a -> Int

instance Weigh Token where
  weigh (Token n) = n * 2

shared :: String
shared = "Base.shared"

twice :: Int -> Int
twice = (* 2)
