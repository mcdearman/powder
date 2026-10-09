-- Haskell 2010 Report 4.5.5 Rule 1: `plus = (+)` is a simple pattern binding without a signature, so it is
-- not generalised over the constrained type variable and cannot be used at both Int and Double
module Main (main) where

plus = (+)

main :: IO ()
main = do
  print (plus (1 :: Int) 2)
  print (plus (1.5 :: Double) 2)
