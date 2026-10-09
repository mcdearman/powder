-- Haskell 2010 Report 10.3: an explicit close brace cannot close an implicit layout block (L (} : ts) (m : ms) with m > 0 is a parse error)
module Main (main) where

f :: Int -> Int
f x = y where
  y = x }

main :: IO ()
main = print (f 1)
