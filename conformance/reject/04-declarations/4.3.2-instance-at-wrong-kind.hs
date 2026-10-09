-- Haskell 2010 Report 4.3.2 / 4.6: the instance type must have the kind of the class variable;
-- Functor's parameter has kind * -> * but Int has kind *
module Main (main) where

instance Functor Int

main :: IO ()
main = putStrLn "unreachable"
