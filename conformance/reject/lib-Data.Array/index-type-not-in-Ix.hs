-- Haskell 2010 Report 14, 19: listArray :: Ix i => (i, i) -> [e] -> Array i e; Double is not an
-- instance of Ix, so an array cannot be indexed by Double
module Main (main) where

import Data.Array

table :: Array Double Char
table = listArray (0.0, 2.0) "abc"

main :: IO ()
main = print (elems table)
