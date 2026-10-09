-- Haskell 2010 Report 5.3.1: only the entities named in an import list are imported
module Main (main) where

import Lib (visible)

main :: IO ()
main = print (visible + hiddenToo)
