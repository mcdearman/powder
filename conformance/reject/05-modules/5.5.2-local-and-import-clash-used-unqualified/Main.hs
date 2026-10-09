-- Haskell 2010 Report 5.5.2: a top-level declaration and an import with the same name clash when referenced unqualified
module Main (main) where

import Lib

visible :: Int
visible = 5

main :: IO ()
main = print visible
