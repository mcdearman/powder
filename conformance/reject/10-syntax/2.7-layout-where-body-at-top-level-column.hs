-- Haskell 2010 Report 10.3: a where block must be indented more than the enclosing block; here the where block is empty and
-- `y = x` becomes a new top-level declaration in which x is not in scope
module Main (main) where

f :: Int -> Int
f x = x + y
  where
y :: Int
y = x

main :: IO ()
main = print (f 1)
