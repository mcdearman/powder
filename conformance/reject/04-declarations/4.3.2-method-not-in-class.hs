-- Haskell 2010 Report 4.3.2: an instance declaration may only bind the methods of its class; `other` is not a method of C
module Main (main) where

class C a where
  c :: a -> Int

data T = T

instance C T where
  c _ = 0
  other _ = 1

main :: IO ()
main = putStrLn "unreachable"
