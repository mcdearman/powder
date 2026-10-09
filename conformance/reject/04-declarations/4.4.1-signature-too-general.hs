-- Haskell 2010 Report 4.4.1: a signature may not be more general than the principal type;
-- the body needs Num a, which `a -> a` does not provide
module Main (main) where

f :: a -> a
f x = x + 1

main :: IO ()
main = print (f (1 :: Int))
