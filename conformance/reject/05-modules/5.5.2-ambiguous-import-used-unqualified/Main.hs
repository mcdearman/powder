-- Haskell 2010 Report 5.5.2: an unqualified reference to a name imported from two modules as different entities is ambiguous
module Main (main) where

import Lib
import Other

main :: IO ()
main = print visible
