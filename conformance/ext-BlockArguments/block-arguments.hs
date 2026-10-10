{-# LANGUAGE BlockArguments #-}
-- BlockArguments: a do, a lambda, a case, an if or a let is an argument as it stands
module Main (main) where

import Control.Monad (forM_, when)

twice :: (a -> a) -> a -> a
twice f = f . f

main :: IO ()
main = do
  forM_ [1, 2, 3 :: Int] \n -> do
    when (odd n) do
      putStrLn ("odd " ++ show n)
  print (twice (\x -> x * 2) (5 :: Int), twice (+ 1) 5 :: Int)
  print (($ (5 :: Int)) \x -> x + 1)
  print (map (\x -> x + 1) [1, 2 :: Int], foldr (\x acc -> x : acc) [] "abc")
  print (maybe 0 succ case lookup 'a' [('a', 1 :: Int)] of Just n -> Just n; Nothing -> Nothing)
  print (id if True then 'y' else 'n', id let k = 3 :: Int in k * k)
  r <- return do 4 + 5
  print (r :: Int)
  mapM_ print do
    x <- [1, 2 :: Int]
    return (x * 10)
