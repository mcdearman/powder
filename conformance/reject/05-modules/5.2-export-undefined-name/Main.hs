-- Haskell 2010 Report 5.2: entities in an export list must be in scope in the module
module Main (main, doesNotExist) where

import Lib

main :: IO ()
main = print visible
