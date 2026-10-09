-- Haskell 2010 Report 10.3: a nested block must be indented more than the enclosing one (no NondecreasingIndentation)
module Main (main) where

main :: IO ()
main = do
  r <- do
  return (1 :: Int)
  print r
