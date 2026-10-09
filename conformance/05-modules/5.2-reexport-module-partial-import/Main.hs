-- Haskell 2010 Report 5.2: `module B` exports only entities in scope with BOTH unqualified name e and qualified name B.e
module Main (main) where

import A

-- A imports only `x` unqualified from B, so `module B` re-exports x but not y or z; these do not clash
y :: String
y = "Main.y"

z :: String
z = "Main.z"

-- A imports C only qualified, so `module C` exports nothing
c :: String
c = "Main.c"

-- A imports D with an alias: `module DD` (the alias) exports d
main :: IO ()
main = do
  print ("x re-exported", x)
  print ("y not re-exported", y)
  print ("z not re-exported", z)
  print ("c not re-exported", c)
  print ("d re-exported via alias", d)
  print ("A's own", useAll)
