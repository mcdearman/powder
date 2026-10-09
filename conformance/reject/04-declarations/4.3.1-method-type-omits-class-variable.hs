-- Haskell 2010 Report 4.3.1: the type of every class method must mention the class type variable
module Main (main) where

class C a where
  size :: Int

main :: IO ()
main = putStrLn "unreachable"
