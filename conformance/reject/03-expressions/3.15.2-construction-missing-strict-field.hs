-- Haskell 2010 Report 3.15.2 (REJECT): a strict field may not be omitted in a construction using field labels
module Main (main) where

data S = S { sx :: !Int, sy :: Int }

main :: IO ()
main = print (sy (S { sy = 1 }))
