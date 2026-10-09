-- Haskell 2010 Report 2.4: reserved words (here `of`) cannot be used as variable identifiers
module Main (main) where

main :: IO ()
main = do
  let of = 1 :: Int
  print of
