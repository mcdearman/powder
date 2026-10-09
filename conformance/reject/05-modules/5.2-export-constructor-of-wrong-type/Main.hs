-- Haskell 2010 Report 5.2: in T(c1,...,cn) each ci must be a constructor or field of T
module Main (main, Mine(MkT)) where

import Lib

data Mine = MkMine

main :: IO ()
main = print visible
