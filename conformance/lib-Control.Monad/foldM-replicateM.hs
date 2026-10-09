-- Haskell 2010 Report 13 (Control.Monad): foldM, foldM_, replicateM, replicateM_
module Main (main) where

import Control.Monad

step :: Int -> Int -> Maybe Int
step acc x = if x == 0 then Nothing else Just (acc `div` x)

branch :: Int -> Int -> [Int]
branch a x = [a + x, a * x]

main :: IO ()
main = do
  print ("foldM maybe ok", foldM step 100 [2, 5])
  print ("foldM maybe fail", foldM step 100 [2, 0, 5])
  print ("foldM empty", foldM step 100 [])
  print ("foldM list", foldM branch 1 [2, 3])
  print ("foldM is left to right", foldM (\acc c -> Just (acc ++ [c])) "" "abc")
  total <- foldM (\acc x -> do { putStrLn ("acc=" ++ show acc ++ " x=" ++ show x); return (acc + x) }) 0 [1, 2, 3 :: Int]
  print ("foldM io", total)
  foldM_ (\acc x -> do { print (acc * x); return (acc * x) }) 1 [2, 3, 4 :: Int]
  print ("foldM_ maybe ok", foldM_ step 8 [2, 2])
  print ("foldM_ maybe fail", foldM_ step 8 [0])
  print ("replicateM list", replicateM 2 "ab")
  print ("replicateM list 3", length (replicateM 3 [0, 1 :: Int]))
  print ("replicateM zero", replicateM 0 "ab")
  print ("replicateM negative", replicateM (-1) "ab")
  print ("replicateM just", replicateM 3 (Just 'x'))
  print ("replicateM nothing", replicateM 2 (Nothing :: Maybe Char))
  print ("replicateM zero nothing", replicateM 0 (Nothing :: Maybe Char))
  cs <- replicateM 2 (putStrLn "effect" >> return 'q')
  print ("replicateM io", cs)
  replicateM_ 3 (putStrLn "tick")
  replicateM_ 0 (putStrLn "never")
  print ("replicateM_ just", replicateM_ 2 (Just 'x'))
  print ("replicateM_ nothing", replicateM_ 2 (Nothing :: Maybe Char))
  print ("replicateM_ list", replicateM_ 2 "ab")
