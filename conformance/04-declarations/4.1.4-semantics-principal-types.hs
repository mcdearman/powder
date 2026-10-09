-- Haskell 2010 Report 4.1.4: principal types are inferred for unannotated bindings and used at many instances
module Main (main) where

-- No type signatures on purpose: each binding gets its principal type.
twice f x = f (f x)

pairUp x y = (x, y)

double x = x + x

compose3 f g h = f . g . h

largest x y = if x >= y then x else y

describe x = show x ++ "!"

applyAll fs x = map (\f -> f x) fs

swap3 (a, b, c) = (c, b, a)

main :: IO ()
main = do
  print ("twice int", twice (+ 1) (0 :: Int))
  print ("twice string", twice ('x' :) "")
  print ("twice twice", twice twice (* 2) (1 :: Int))
  print ("pairUp", pairUp 'a' True, pairUp (1 :: Int) "s")
  print ("double int", double (21 :: Int))
  print ("double double", double (1.25 :: Double))
  print ("double integer", double (2 ^ (64 :: Int) :: Integer))
  print ("compose3", compose3 show (+ 1) (* 2) (5 :: Int))
  print ("compose3 lists", compose3 length (map not) (filter id) [True, False, True])
  print ("largest", largest (3 :: Int) 4, largest 'a' 'Z', largest "ab" "b")
  print ("largest tuples", largest (1 :: Int, 'b') (1, 'a'))
  print ("describe", describe (1 :: Int), describe True, describe "s")
  print ("applyAll", applyAll [(+ 1), (* 2)] (10 :: Int))
  print ("applyAll strings", applyAll [reverse, take 1] "abc")
  print ("swap3", swap3 (1 :: Int, 'b', True))
