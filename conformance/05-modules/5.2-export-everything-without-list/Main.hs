-- Haskell 2010 Report 5.2: with no export list, all entities declared in the module are exported, but imported ones are not
module Main (main) where

import A
import Data.Char (toUpper)

-- A imports Data.List (sort) but does not re-export it; this local sort does not clash
sort :: [Int] -> [Int]
sort = id

main :: IO ()
main = do
  print ("value", aValue)
  print ("function", aFunction 2)
  print ("operator with fixity", 1 |+| 2 |+| 3)
  print ("type and constructors", [Red, Green], Wrap 3)
  print ("field", unwrap (Wrap 4), (Wrap 1) { unwrap = 9 })
  print ("synonym", (5 :: Number))
  print ("class and method", describe Red, describe (Wrap 1))
  print ("uses A's private import internally", sortedInA)
  print ("local sort unaffected", sort [3, 1, 2])
  print ("own import", map toUpper "ok")
