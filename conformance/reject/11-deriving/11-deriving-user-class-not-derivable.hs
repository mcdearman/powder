-- Haskell 2010 Report 11 (and 4.3.3): REJECT. Only the classes Eq, Ord, Enum, Bounded, Show, Read (and Ix) can appear
-- in a deriving clause; a user-defined class cannot be derived.
module Main (main) where

class Describable a where
  describe :: a -> String

data T = T deriving (Describable)

main :: IO ()
main = return ()
