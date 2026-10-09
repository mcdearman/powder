-- Haskell 2010 Report 4.2.1: every type variable on the right-hand side of a data declaration must be a
-- parameter of the type (no existential types); `b` is not bound
module Main (main) where

data T a = MkT a b

main :: IO ()
main = putStrLn "unreachable"
