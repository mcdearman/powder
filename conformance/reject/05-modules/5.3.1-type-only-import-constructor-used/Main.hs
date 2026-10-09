-- Haskell 2010 Report 5.3.1: importing T without (..) or a constructor list does not import its constructors
module Main (main) where

import Lib (T, C(..))

main :: IO ()
main = print (method (MkT 1))
