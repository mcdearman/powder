-- Haskell 2010 Report 5.2: entities not in the export list are not in scope in importing modules
module Main (main) where

import Lib

main :: IO ()
main = print (visible + notExported)
