-- Haskell 2010 Report 19.2, 11: Ix may be derived only for enumerations (all constructors nullary) and
-- for single-constructor types; this type has several constructors, one of them with a field
module Main (main) where

import Data.Ix

data Shape = Dot | Line Int
  deriving (Eq, Ord, Show, Ix)

main :: IO ()
main = print (range (Dot, Line 2))
