-- Haskell 2010 Report 13 (Control.Monad): liftM, liftM2, liftM3, liftM4, liftM5, ap
module Main (main) where

import Control.Monad

add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c

add4 :: Int -> Int -> Int -> Int -> Int
add4 a b c d = a + b + c + d

add5 :: Int -> Int -> Int -> Int -> Int -> Int
add5 a b c d e = a + b + c + d + e

say :: String -> Int -> IO Int
say s n = putStrLn s >> return n

main :: IO ()
main = do
  print ("liftM just", liftM (+ 1) (Just (1 :: Int)))
  print ("liftM nothing", liftM (+ 1) (Nothing :: Maybe Int))
  print ("liftM list", liftM show [1, 2 :: Int])
  print ("liftM2 maybe", liftM2 (+) (Just 1) (Just (2 :: Int)))
  print ("liftM2 nothing", liftM2 (+) (Just 1) (Nothing :: Maybe Int))
  print ("liftM2 list order", liftM2 (+) [1, 2] [10, 20 :: Int])
  print ("liftM2 pairs", liftM2 (,) "ab" [True, False])
  print ("liftM3 maybe", liftM3 add3 (Just 1) (Just 2) (Just 3))
  print ("liftM3 nothing", liftM3 add3 (Just 1) Nothing (Just 3))
  print ("liftM3 list", liftM3 (,,) "ab" "c" "de")
  print ("liftM4 maybe", liftM4 add4 (Just 1) (Just 2) (Just 3) (Just 4))
  print ("liftM4 list", liftM4 add4 [1, 2] [10] [100] [1000, 2000])
  print ("liftM5 maybe", liftM5 add5 (Just 1) (Just 2) (Just 3) (Just 4) (Just 5))
  print ("liftM5 nothing", liftM5 add5 (Just 1) (Just 2) (Just 3) (Just 4) Nothing)
  print ("liftM5 list", liftM5 (,,,,) "a" "b" "cd" "e" "f")
  x <- liftM2 (-) (say "first" 10) (say "second" 3)
  print ("liftM2 io left to right", x)
  y <- liftM3 add3 (say "a" 1) (say "b" 2) (say "c" 3)
  print ("liftM3 io", y)
  print ("ap maybe", Just (+ 1) `ap` Just (2 :: Int))
  print ("ap nothing function", (Nothing :: Maybe (Int -> Int)) `ap` Just 2)
  print ("ap nothing argument", Just (+ 1) `ap` (Nothing :: Maybe Int))
  print ("ap list order", [(+ 1), (* 2)] `ap` [10, 20 :: Int])
  print ("ap chain", return add3 `ap` Just 1 `ap` Just 2 `ap` Just 3)
  z <- return (* 2) `ap` say "ap io" 21
  print ("ap io", z)
  print ("liftM2 is ap", liftM2 (+) [1, 2] [3, 4 :: Int] == (return (+) `ap` [1, 2] `ap` [3, 4]))
