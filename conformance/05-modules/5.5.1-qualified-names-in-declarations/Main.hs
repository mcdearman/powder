-- Haskell 2010 Report 5.5.1 / 4.3.2: binding occurrences are never qualified: instance method bindings and record-pattern
-- fields for qualified-only imports; uses of the class, type and method names are qualified
module Main (main) where

import qualified Shape as S
import qualified Data.List

data Tri = Tri Int Int Int

-- the class and its methods are in scope only as S.Measure / S.area / S.label, yet the bindings are unqualified
instance S.Measure Tri where
  area (Tri a b c) = a + b + c
  label _ = "tri"

instance S.Measure S.Sq where
  area (S.Sq n) = n * n

instance Show Tri where
  show (Tri a b c) = "Tri" ++ concat (Data.List.intersperse "-" (map show [a, b, c]))

total :: (S.Measure a, S.Measure b) => a -> b -> Int
total x y = S.area x + S.area y

main :: IO ()
main = do
  print ("instance for local type of a qualified class", S.area (Tri 1 2 3), S.label (Tri 1 2 3))
  print ("instance for qualified type; default method", S.area (S.Sq 4), S.label (S.Sq 4))
  print ("qualified class in context", total (Tri 1 1 1) (S.Sq 2))
  print ("show", Tri 3 4 5)
  print ("qualified function using the instances", S.report (Tri 2 2 2))
  print ("Main-qualified local names", Main.total (S.Sq 1) (S.Sq 1))
