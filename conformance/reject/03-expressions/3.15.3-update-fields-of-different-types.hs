-- Haskell 2010 Report 3.15.3 (REJECT): all labels in an update must belong to one datatype, and some constructor must define them all
module Main (main) where

data P = P { px :: Int }

data Q = Q { qx :: Int }

main :: IO ()
main = print (px ((P 1) { px = 2, qx = 3 }))
