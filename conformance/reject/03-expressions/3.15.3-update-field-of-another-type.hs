-- Haskell 2010 Report 3.15.3 (REJECT): updating a value with a field label of a different datatype is a type error
module Main (main) where

data P = P { px :: Int }

data Q = Q { qx :: Int }

main :: IO ()
main = print (qx ((P 1) { qx = 2 }))
