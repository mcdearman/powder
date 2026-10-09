-- Haskell 2010 Report 5.5.2: two imported entities with the same name only clash if the unqualified name is actually used;
-- references are disambiguated by qualification
module Main (main) where

import A
import B
import qualified A as First
import qualified B as Second

-- a local definition with the same name as an import: also only an error if referenced unqualified
value :: String
value = "Main.value"

main :: IO ()
main = do
  print ("functions", A.name, B.name)
  print ("types and constructors", A.Item 1, B.Item "one")
  print ("operators", 5 A.<+> 3, 5 B.<+> 3)
  print ("methods of same-named classes", A.render (A.Item 2), B.render (B.Item "two"))
  print ("fields", A.payload (A.Item 3), B.payload (B.Item "three"))
  print ("aliases", First.name, Second.name)
  print ("unclashing names stay unqualified", onlyA, onlyB)
  print ("local vs imported", Main.value, A.value, B.value)
  print ("signature with qualified types", describe (A.Item 4) (B.Item "four"))

describe :: A.Item -> B.Item -> String
describe a b = A.render a ++ "/" ++ B.render b
