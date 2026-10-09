-- Haskell 2010 Report 4.3.2: an instance head must be built from a type constructor; a bare type variable is not allowed
module Main (main) where

class C a where
  c :: a -> Int

instance C a where
  c _ = 0

main :: IO ()
main = putStrLn "unreachable"
