-- Haskell 2010 Report 5.2: the same entity may be named several times in an export list (directly, via T(..), via module M)
module Main (main) where

import A

main :: IO ()
main = do
  print ("value exported twice", v)
  print ("type exported as T, T(..) and T(C)", [C1, C2])
  print ("re-export overlapping with explicit export", fromB)
  print ("class exported twice", m C1, m C2)
