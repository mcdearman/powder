-- Haskell 2010 Report 5.4: an instance is only in scope if a module in the import chain declares it; none does for C Bool
module Main (main) where

import Lib

main :: IO ()
main = print (method True)
