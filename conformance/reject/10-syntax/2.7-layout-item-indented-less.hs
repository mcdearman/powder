-- Haskell 2010 Report 10.3: a line indented less than the current block closes it; the second binding is not part of the let
module Main (main) where

main :: IO ()
main = do
  let a = 1 :: Int
    b = 2 :: Int
  print (a + b)
