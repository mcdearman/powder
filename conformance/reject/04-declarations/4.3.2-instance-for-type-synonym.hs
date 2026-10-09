-- Haskell 2010 Report 4.3.2: the type in an instance head must not be a type synonym
module Main (main) where

type Flag = Bool

class C a where
  c :: a -> Int

instance C Flag where
  c _ = 0

main :: IO ()
main = putStrLn "unreachable"
