-- Haskell 2010 Report 13 (Control.Monad): MonadPlus(mzero, mplus) for Maybe and lists, msum, guard
module Main (main) where

import Control.Monad

triples :: Int -> [(Int, Int, Int)]
triples n = do
  a <- [1 .. n]
  b <- [a .. n]
  c <- [b .. n]
  guard (a * a + b * b == c * c)
  return (a, b, c)

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = do
  guard (y /= 0)
  return (x `div` y)

main :: IO ()
main = do
  print ("mzero maybe", (mzero :: Maybe Int))
  print ("mzero list", (mzero :: [Int]))
  print ("mplus just just", Just 1 `mplus` Just (2 :: Int))
  print ("mplus nothing just", Nothing `mplus` Just (2 :: Int))
  print ("mplus just nothing", Just (1 :: Int) `mplus` Nothing)
  print ("mplus nothing nothing", (Nothing :: Maybe Int) `mplus` Nothing)
  print ("mplus lists", [1, 2] `mplus` [3 :: Int])
  print ("mplus mzero left", (mzero `mplus` [1, 2 :: Int]) == [1, 2])
  print ("mplus mzero right", (Just 'a' `mplus` mzero) == Just 'a')
  print ("mzero bind", ((mzero :: [Int]) >>= \x -> [x, x]))
  print ("then mzero", ([1, 2, 3 :: Int] >> (mzero :: [Char])))
  print ("msum maybe", msum [Nothing, Just 1, Just (2 :: Int)])
  print ("msum all nothing", msum [Nothing, Nothing :: Maybe Int])
  print ("msum empty", msum ([] :: [Maybe Int]))
  print ("msum lists", msum [[1], [2, 3], [], [4 :: Int]])
  print ("guard true maybe", (guard True :: Maybe ()))
  print ("guard false maybe", (guard False :: Maybe ()))
  print ("guard true list", (guard True :: [()]))
  print ("guard false list", (guard False :: [()]))
  print ("triples", triples 20)
  print ("safeDiv", safeDiv 7 2)
  print ("safeDiv zero", safeDiv 7 0)
  print ("mplus is lazy for Just", Just 'a' `mplus` undefined)
