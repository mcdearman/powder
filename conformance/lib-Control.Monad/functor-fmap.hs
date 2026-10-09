-- Haskell 2010 Report 13 (Control.Monad): Functor(fmap) at list, Maybe and IO
module Main (main) where

import Control.Monad

double :: Int -> Int
double x = x * 2

main :: IO ()
main = do
  print ("list", fmap (+ 1) [1, 2, 3 :: Int])
  print ("empty list", fmap (+ 1) ([] :: [Int]))
  print ("string", fmap succ "HAL")
  print ("just", fmap show (Just (5 :: Int)))
  print ("nothing", fmap show (Nothing :: Maybe Int))
  n <- fmap length (return "hello" :: IO String)
  print ("io", n)
  s <- fmap (map double) (return [1, 2, 3])
  print ("io list", s)
  print ("identity law list", fmap id [1, 2, 3 :: Int] == [1, 2, 3])
  print ("identity law maybe", fmap id (Just 'x') == Just 'x')
  print ("composition law", fmap (negate . double) [1, 2, 3] == (fmap negate . fmap double) [1, 2, 3])
  print ("nested", fmap (fmap succ) [Just 'a', Nothing, Just 'y'])
  print ("changes type", fmap even [1, 2, 3 :: Int])
  print ("lazy in elements", length (fmap (\_ -> (undefined :: Int)) "abc"))
  print ("fmap is map", fmap double [4, 5] == map double [4, 5])
