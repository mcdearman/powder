-- Haskell 2010 Report 20.1 (Data.List): basic functions (++), head, last, tail, init, null, length
module Main (main) where

import Data.List

xs :: [Int]
xs = [1, 2, 3, 4, 5]

none :: [Int]
none = []

main :: IO ()
main = do
  print ("append", xs ++ [6, 7])
  print ("append empty left", none ++ xs)
  print ("append empty right", xs ++ none)
  print ("append both empty", none ++ none)
  print ("append strings", "foo" ++ "bar" ++ "baz")
  print ("append is right associative", [1] ++ [2] ++ [3 :: Int])
  print ("append binds looser than cons", 1 : [2] ++ [3 :: Int])
  print ("append to infinite", take 5 (xs ++ [10 ..]))
  print ("append infinite on left", take 3 ([1 ..] ++ xs))
  print ("append is lazy in second argument", take 2 (xs ++ undefined))
  print ("head", head xs)
  print ("head singleton", head "x")
  print ("head infinite", head [7 :: Int ..])
  print ("head ignores tail", head (1 : undefined :: [Int]))
  print ("last", last xs)
  print ("last singleton", last "x")
  print ("tail", tail xs)
  print ("tail singleton", tail "x")
  print ("tail infinite", take 3 (tail [1 :: Int ..]))
  print ("init", init xs)
  print ("init singleton", init "x")
  print ("init is lazy", take 3 (init [1 :: Int ..]))
  print ("null", null none, null xs, null "", null "a")
  print ("null infinite", null [1 :: Int ..])
  print ("null ignores elements", null [undefined :: Int])
  print ("length", length none, length xs, length "hello")
  print ("length ignores elements", length [undefined, undefined :: Int])
  print ("length nested", length [[1, 2], [], [3 :: Int]], map length [[1, 2], [], [3 :: Int]])
  print ("head : tail", head xs : tail xs == xs)
  print ("init ++ [last]", init xs ++ [last xs] == xs)
  print ("length of append", length (xs ++ xs) == 2 * length xs)
