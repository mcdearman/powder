-- Haskell 2010 Report 5.2: a non-exported entity cannot be reached by a qualified name either
module Main (main) where

import qualified Lib

main :: IO ()
main = print (Lib.visible + Lib.notExported)
