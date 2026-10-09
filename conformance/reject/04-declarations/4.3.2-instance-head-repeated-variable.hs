-- Haskell 2010 Report 4.3.2: the type variables in an instance head must be distinct; (a, a) repeats one
module Main (main) where

class C a where
  c :: a -> Int

instance C (a, a) where
  c _ = 0

main :: IO ()
main = putStrLn "unreachable"
