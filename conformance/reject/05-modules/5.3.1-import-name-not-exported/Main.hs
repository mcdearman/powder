-- Haskell 2010 Report 5.3.1: it is an error to import an entity the module does not export
module Main (main) where

import Lib (visible, notExported)

main :: IO ()
main = print visible
