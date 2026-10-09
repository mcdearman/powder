-- Haskell 2010 Report 5.5.2: an entity imported by two routes (directly and through re-exports) is the same entity: no clash
module Main (main) where

import Base
import Left
import Right
import qualified Left as L
import qualified Right as R

main :: IO ()
main = do
  print ("unqualified use of an entity imported three ways", shared, twice 4)
  print ("type and constructor imported three ways", Token 1, [Token 2] == [Token 2])
  print ("class method imported three ways", weigh (Token 5))
  print ("all qualifiers name the same entity", L.shared == R.shared, L.twice 1 == R.twice 1)
  print ("module-specific names", leftOnly, rightOnly)
  print ("values built in different modules are compatible", leftToken == rightToken, [leftToken, rightToken, Token 0])
