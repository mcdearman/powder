-- Haskell 2010 Report 4.5.1, 4.5.2: bindings that refer to each other are one group, generalised together
-- and then usable at several types; a value defined by way of itself
module Main (main) where

ping, pong :: Int -> a -> [a]
ping n x = if n <= 0 then [] else x : pong (n - 1) x
pong n x = if n <= 0 then [] else x : ping (n - 1) x

evens, odds :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs
odds [] = []
odds (_ : xs) = evens xs

data Stream = Stream Int Stream

ones :: Stream
ones = Stream 1 ones

heads :: Int -> Stream -> [Int]
heads n (Stream x rest) = if n <= 0 then [] else x : heads (n - 1) rest

main :: IO ()
main = do
  print (ping 3 'a', pong 2 "a")
  print (evens "abcdefg", odds [1 .. 7 :: Int])
  print (heads 4 ones)
  let countDown, countUp :: Int -> [Int]
      countDown n = if n < 0 then [] else n : countUp (n - 2)
      countUp n = if n < 0 then [] else n : countDown (n + 1)
  print (take 8 (countDown 5))
