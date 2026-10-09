-- Haskell 2010 Report 5.2: `module M` in an export list re-exports everything imported from M (in scope unqualified and qualified)
module Main (main) where

import Facade

main :: IO ()
main = do
  print ("from Inner via Facade", innerValue, innerFn 2)
  print ("type via Facade", [InA, InB])
  print ("class via Facade", tag InA, tag InB)
  print ("Facade's own", facadeValue)
  print ("from Other via Facade", otherValue)
