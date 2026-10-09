-- Haskell 2010 Report 4.3.4: a default declaration affects only the module it appears in and is neither exported nor imported
module Main (main) where

import IntDefault (inIntModule, polyShow)
import DoubleDefault (inDoubleModule)

-- Main has no default declaration: default (Integer, Double) applies here
main :: IO ()
main = do
  print ("Main defaults to Integer", 2 ^ 64)
  print ("Main defaults fractional to Double", 7 / 2)
  print ("module with default (Int)", inIntModule)
  print ("module with default (Double)", inDoubleModule)
  print ("exported polymorphic function is not affected by its module's default", polyShow 3, polyShow 2.5)
