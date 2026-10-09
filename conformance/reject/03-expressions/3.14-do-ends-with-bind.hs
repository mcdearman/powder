-- Haskell 2010 Report 3.14 (REJECT): the last statement of a do expression must be an expression
module Main (main) where

main :: IO ()
main = do
  putStrLn "start"
  x <- return ()
