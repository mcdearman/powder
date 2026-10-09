-- Haskell 2010 Report 4.3.3: only Eq, Ord, Enum, Bounded, Show, Read (and Ix) can be derived, not a user-defined class
module Main (main) where

class C a where
  c :: a -> Int
  c _ = 0

data T = T deriving (C)

main :: IO ()
main = putStrLn "unreachable"
