-- Haskell 2010 Report 4.3.2: an instance head must be a type constructor applied to distinct type
-- variables; `Maybe Int` has a non-variable argument
module Main (main) where

class C a where
  c :: a -> Int

instance C (Maybe Int) where
  c _ = 0

main :: IO ()
main = putStrLn "unreachable"
