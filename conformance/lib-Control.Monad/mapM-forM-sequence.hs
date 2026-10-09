-- Haskell 2010 Report 13 (Control.Monad): mapM, mapM_, forM, forM_, sequence, sequence_
module Main (main) where

import Control.Monad

positive :: Int -> Maybe Int
positive x = if x > 0 then Just x else Nothing

main :: IO ()
main = do
  rs <- mapM (\x -> do { putStrLn ("visit " ++ show x); return (x * 2) }) [1, 2, 3 :: Int]
  print ("mapM io", rs)
  mapM_ (\c -> putStrLn ['<', c, '>']) "abc"
  mapM_ print ([] :: [Int])
  qs <- forM "xy" (\c -> do { putStrLn ("forM " ++ [c]); return (fromEnum c) })
  print ("forM io", qs)
  forM_ [1, 2 :: Int] (\i -> forM_ "ab" (\c -> print (i, c)))
  print ("mapM maybe ok", mapM positive [1, 2, 3])
  print ("mapM maybe fail", mapM positive [1, 0, 3])
  print ("mapM maybe empty", mapM positive [])
  print ("mapM list", mapM (\x -> [x, x + 10]) [1, 2 :: Int])
  print ("forM maybe", forM [3, 4] positive)
  print ("mapM_ maybe ok", mapM_ positive [1, 2])
  print ("mapM_ maybe fail", mapM_ positive [1, -2])
  print ("forM_ list", forM_ [1, 2 :: Int] (\x -> [x, x]))
  print ("sequence maybe", sequence [Just 1, Just (2 :: Int)])
  print ("sequence maybe nothing", sequence [Just 1, Nothing, Just (3 :: Int)])
  print ("sequence empty", sequence ([] :: [Maybe Int]))
  print ("sequence lists", sequence [[1, 2], [3, 4 :: Int]])
  print ("sequence with empty list", sequence [[1, 2], [], [3 :: Int]])
  ss <- sequence [return 'a', putStrLn "effect" >> return 'b', return 'c']
  print ("sequence io", ss)
  sequence_ [putStrLn "one", putStrLn "two"]
  sequence_ ([] :: [IO ()])
  print ("sequence_ maybe", sequence_ [Just 'a', Just 'b'])
  print ("sequence_ nothing", sequence_ [Just 'a', Nothing])
  print ("sequence_ lists", sequence_ [[1, 2], [3, 4 :: Int]])
