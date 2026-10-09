-- Haskell 2010 Report 4.3.2: if T is declared an instance of a class, it must also be an instance of
-- each superclass; Ord T is declared without Eq T
module Main (main) where

data T = T

instance Ord T where
  compare _ _ = EQ

main :: IO ()
main = putStrLn "unreachable"
