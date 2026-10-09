-- Haskell 2010 Report 3.14: do in IO; statement order, explicit braces, empty statements, control flow inside do
module Main (main) where

import Control.Monad (when, unless)

twice :: IO a -> IO (a, a)
twice act = do
  a <- act
  b <- act
  return (a, b)

loop :: Int -> IO Int
loop n = do
  if n >= 3
    then return n
    else do
      putStrLn ("loop " ++ show n)
      loop (n + 1)

main :: IO ()
main = do
  putStrLn "first"
  putStrLn "second"
  x <- return 1
  y <- return (x + 1)
  print ("bind", x, y)
  t <- twice (putStrLn "action runs twice" >> return 'r')
  print ("twice", t)
  do { putStrLn "braces 1"; putStrLn "braces 2" }
  do { ; putStrLn "empty statements"; ; ; putStrLn "are allowed"; }
  n <- loop 0
  print ("loop result", n)
  when (n == 3) (putStrLn "when true")
  when (n /= 3) (putStrLn "when false")
  unless (n /= 3) $ putStrLn "unless false"
  r <- case n of
    3 -> do
      putStrLn "case in do"
      return "three"
    _ -> return "other"
  print ("case result", r)
  mapM_ (\i -> putStrLn ("mapM_ " ++ show i)) [1, 2, 3]
  rs <- mapM (\i -> do { putStrLn ("mapM " ++ show i); return (i * i) }) [1, 2, 3]
  print ("mapM results", rs)
  let action = putStrLn "defined once, run twice"
  action
  action
  _ <- return (undefined :: Int)
  putStrLn "bound bottom is never forced"
  putStrLn "a" >> putStrLn "b" >>= \u -> print ("unit from >>=", u)
  return ()
  putStrLn "return does not exit the block"
