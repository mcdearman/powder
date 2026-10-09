-- Haskell 2010 Report 5.3: an imported module must exist
module Main (main) where

import Lib
import NoSuchModuleAnywhere

main :: IO ()
main = print visible
