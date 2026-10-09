-- Haskell 2010 Report 4.5.5 Rule 1: a non-simple pattern binding is always restricted, so `lo` (constrained
-- by Bounded) is monomorphic and cannot be used at both Int and Char
module Main (main) where

(lo, hi) = (minBound, maxBound)

main :: IO ()
main = do
  print (lo :: Int)
  print (lo :: Char)
  print (hi :: Bool)
