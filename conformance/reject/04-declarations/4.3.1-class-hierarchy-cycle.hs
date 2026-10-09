-- Haskell 2010 Report 4.3.1: the superclass relation must not be cyclic
module Main (main) where

class B a => A a where
  a :: a -> Int

class A a => B a where
  b :: a -> Int

main :: IO ()
main = putStrLn "unreachable"
