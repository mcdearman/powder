-- Haskell 2010 Report 13 (Control.Monad): filterM (incl. powerset), mapAndUnzipM, zipWithM, zipWithM_
module Main (main) where

import Control.Monad

divide :: Int -> Int -> Maybe Int
divide x y = if y /= 0 then Just (x `div` y) else Nothing

main :: IO ()
main = do
  print ("powerset", filterM (const [True, False]) [1, 2, 3 :: Int])
  print ("powerset empty", filterM (const [True, False]) ([] :: [Int]))
  print ("powerset other order", filterM (const [False, True]) "ab")
  print ("filterM maybe", filterM (\x -> Just (x > 1)) [1, 2, 3 :: Int])
  print ("filterM maybe fail", filterM (\x -> if x == 2 then Nothing else Just True) [1, 2, 3 :: Int])
  kept <- filterM (\x -> do { putStrLn ("test " ++ show x); return (odd x) }) [1, 2, 3, 4, 5 :: Int]
  print ("filterM io", kept)
  print ("mapAndUnzipM maybe", mapAndUnzipM (\x -> Just (x, x * x)) [1, 2, 3 :: Int])
  print ("mapAndUnzipM nothing", mapAndUnzipM (\x -> if x > 2 then Nothing else Just (x, 'c')) [1, 2, 3 :: Int])
  print ("mapAndUnzipM empty", mapAndUnzipM (\x -> Just (x, x)) ([] :: [Int]))
  print ("mapAndUnzipM list", mapAndUnzipM (\x -> [(x, 'a'), (x + 1, 'b')]) [10 :: Int])
  (as, bs) <- mapAndUnzipM (\c -> do { putStrLn ("unzip " ++ [c]); return (c, fromEnum c) }) "AB"
  print ("mapAndUnzipM io", as, bs)
  print ("zipWithM ok", zipWithM divide [10, 20] [2, 5])
  print ("zipWithM fail", zipWithM divide [10, 20] [2, 0])
  print ("zipWithM uneven", zipWithM divide [10, 20, 30] [5])
  print ("zipWithM empty", zipWithM divide [] [1, 2])
  print ("zipWithM list", zipWithM (\x y -> [x, y]) [1, 2] [3, 4 :: Int])
  zs <- zipWithM (\i c -> do { putStrLn (show i ++ [c]); return (i + fromEnum c) }) [1, 2 :: Int] "ab"
  print ("zipWithM io", zs)
  zipWithM_ (\i s -> putStrLn (show i ++ ": " ++ s)) [1 :: Int ..] ["first", "second", "third"]
  print ("zipWithM_ ok", zipWithM_ divide [1, 2] [1, 1])
  print ("zipWithM_ fail", zipWithM_ divide [1, 2] [0, 1])
