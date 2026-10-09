-- Haskell 2010 Report 5.3.1: T(C) may only name constructors that the module exports for T
module Main (main) where

import Lib (T(MkT, Private))

main :: IO ()
main = case MkT 1 of
  MkT n -> print n
