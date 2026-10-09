-- Haskell 2010 Report 3.14 (REJECT): the last statement of a do expression must be an expression, not a let
module Main (main) where

main :: IO ()
main = do
  putStrLn "start"
  let x = ()
