-- Haskell 2010 Report 5.4: instance declarations are not named in export/import lists; all instances in scope are always exported
module Main (main) where

-- Only Top is imported. The type, the class and the instance live in three other modules.
import Top (sample, describeAll)
import Empty ()

main :: IO ()
main = do
  -- Show Thing (derived in Types) and Pretty Thing (defined in Instances) reach Main through Top
  print ("derived Show through chain", sample)
  print ("user instance through chain", describeAll)
  print ("Eq through chain", sample == sample)
