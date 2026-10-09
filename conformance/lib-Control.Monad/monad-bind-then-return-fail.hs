-- Haskell 2010 Report 13 (Control.Monad): Monad((>>=), (>>), return, fail) at list, Maybe and IO
module Main (main) where

import Control.Monad
import System.IO.Error

f :: Int -> [Int]
f x = [x, x * 10]

g :: Int -> [Int]
g x = [x + 1]

main :: IO ()
main = do
  print ("bind list", [1, 2, 3] >>= f)
  print ("bind empty", ([] :: [Int]) >>= f)
  print ("bind just", Just (3 :: Int) >>= \x -> Just (x + 1))
  print ("bind nothing", (Nothing :: Maybe Int) >>= \x -> Just (x + 1))
  print ("bind to nothing", Just (3 :: Int) >>= \_ -> (Nothing :: Maybe Int))
  print ("then list", [1, 2 :: Int] >> "ab")
  print ("then empty", ([] :: [Int]) >> "ab")
  print ("then maybe", Just 'x' >> Just (1 :: Int))
  print ("then nothing", (Nothing :: Maybe Char) >> Just (1 :: Int))
  print ("return list", (return 7 :: [Int]))
  print ("return maybe", (return 7 :: Maybe Int))
  print ("fail maybe", (fail "no" :: Maybe Int))
  print ("fail list", (fail "no" :: [Int]))
  r <- catchIOError (fail "boom" >> return "not reached") (\e -> return (ioeGetErrorString e))
  print ("fail io", r)
  u <- catchIOError (fail "x" >> return False) (\e -> return (isUserError e))
  print ("fail io is user error", u)
  print ("pattern fail maybe", do { (x : _) <- Just ([] :: [Int]); return x })
  print ("pattern ok maybe", do { (x : _) <- Just [4, 5 :: Int]; return x })
  print ("pattern fail list", do { Just x <- [Just 1, Nothing, Just (3 :: Int)]; return x })
  v <- return 5 >>= \w -> return (w * 2 :: Int)
  print ("io bind", v)
  putStr "io " >> putStrLn "then"
  print ("left identity", (return 3 >>= f) == f 3)
  print ("right identity", ([1, 2, 3 :: Int] >>= return) == [1, 2, 3])
  print ("associativity", (([1, 2] >>= f) >>= g) == ([1, 2] >>= (\x -> f x >>= g)))
