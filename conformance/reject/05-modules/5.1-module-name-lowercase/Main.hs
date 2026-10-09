-- Haskell 2010 Report 5.1 / 2.4: module names are conids and must begin with an upper-case letter
module Main (main) where

import Lib
import qualified lowercase.Module as M

main :: IO ()
main = print visible
