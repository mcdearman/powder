-- Haskell 2010 Report 5.5.1: the name being defined by a declaration cannot be qualified
module Main (main) where

import Lib

Main.helper :: Int
Main.helper = visible

main :: IO ()
main = print helper
