-- Haskell 2010 Report 5.3.3: with an as clause, the qualifier is the alias; the original module name is not a valid qualifier
module Main (main) where

import qualified Lib as L

main :: IO ()
main = print (L.visible + Lib.visible)
