-- Haskell 2010 Report 5.1: the module Main must export the value main
module Main (notMain) where

import Lib

notMain :: IO ()
notMain = print visible
