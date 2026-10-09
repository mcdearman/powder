-- Haskell 2010 Report 5.5.3: exported entities may have types mentioning non-exported types/classes; importers can use the
-- values (types are inferred) but cannot name the hidden type
module Main (main) where

import Counter

-- no clash: Counter's type and constructor are not exported
data Counter = MkCounter deriving Show

main :: IO ()
main = do
  let c0 = new
      c2 = tick (tick c0)
      pair = (c0, c2)
  print ("value of hidden type", count c0, count c2)
  print ("hidden type in a local structure", map count [c0, c2, tick c2])
  print ("hidden type in a tuple", count (fst pair), count (snd pair))
  print ("function with hidden-class constraint", describe c2)
  print ("hidden class instance for an exported type", describe (3 :: Int))
  print ("Show instance of hidden type is visible", c2)
  print ("local type with the same name", MkCounter)
