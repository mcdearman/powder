-- Haskell 2010 Report 5.2: export lists may name entities by qualified name; the exported name is the unqualified one
module Main (main) where

import A

main :: IO ()
main = do
  print ("re-exported by qualified name", size [1, 2, 3 :: Int])
  print ("own entity by qualified name", own)
  print ("type by qualified name", Box 1, unBox (Box 2))
  print ("class by qualified name", label (Box 3))
