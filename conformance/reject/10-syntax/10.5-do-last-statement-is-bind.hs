-- Haskell 2010 Report 3.14 / 10.5: the last statement of a do block must be an expression
module Main (main) where

main :: IO ()
main = do
  putStrLn "a"
  x <- return (1 :: Int)
