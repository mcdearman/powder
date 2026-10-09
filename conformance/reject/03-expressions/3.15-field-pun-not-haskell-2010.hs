-- Haskell 2010 Report 3.15 / 3.17.1 (REJECT): a field pattern must have the form label = pattern; punning C {x} is not Haskell 2010
module Main (main) where

data P = P { px :: Int }

getX :: P -> Int
getX P { px } = 0

main :: IO ()
main = print (getX (P 1))
