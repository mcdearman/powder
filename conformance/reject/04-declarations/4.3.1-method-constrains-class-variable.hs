-- Haskell 2010 Report 4.3.1: the context of a class method's type may constrain only type variables
-- other than the class variable; `Eq a` here constrains the class variable itself
module Main (main) where

class C a where
  same :: Eq a => a -> a -> Bool

main :: IO ()
main = putStrLn "unreachable"
