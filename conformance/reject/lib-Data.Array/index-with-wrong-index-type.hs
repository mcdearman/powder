-- Haskell 2010 Report 14: (!) :: Ix i => Array i e -> i -> e; an Array Int Char cannot be indexed with
-- a Char
module Main (main) where

import Data.Array

table :: Array Int Char
table = listArray (0, 2) "abc"

main :: IO ()
main = print (table ! 'a')
