-- Haskell 2010 Report 5.2: 'module M' in an export list requires M to be this module or to be imported by it
module Main (main, module Data.Maybe) where

import Lib

main :: IO ()
main = print visible
