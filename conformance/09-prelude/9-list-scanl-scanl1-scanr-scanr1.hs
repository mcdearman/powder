-- Haskell 2010 Report 9 (PreludeList): scanl, scanl1, scanr, scanr1 on empty, singleton, finite and infinite lists
module Main (main) where

paren :: String -> String -> String
paren a b = "(" ++ a ++ "." ++ b ++ ")"

main :: IO ()
main = do
  print ("scanl", scanl (+) 0 [1, 2, 3, 4 :: Int])
  print ("scanl empty", scanl (+) 0 ([] :: [Int]), scanl (+) 7 ([] :: [Int]))
  print ("scanl singleton", scanl (+) 10 [5 :: Int])
  print ("scanl shape", scanl paren "z" ["a", "b", "c"])
  print ("scanl (-)", scanl (-) 100 [1, 2, 3 :: Int])
  print ("scanl different types", scanl (\n c -> n * 10 + fromEnum c - 48) 0 "123", scanl (flip (:)) [] "abc")
  print ("scanl infinite", take 6 (scanl (+) 0 [1 :: Int ..]), take 5 (scanl (*) 1 [1 :: Integer ..]))
  print ("scanl head needs no list", head (scanl (+) (0 :: Int) undefined))
  print ("scanl length", length (scanl (+) 0 [1 .. 10 :: Int]), last (scanl (+) 0 [1 .. 10 :: Int]) == foldl (+) 0 [1 .. 10])
  print ("scanl1", scanl1 (+) [1, 2, 3, 4 :: Int], scanl1 max [3, 1, 4, 1, 5, 9, 2, 6 :: Int])
  print ("scanl1 empty", scanl1 (+) ([] :: [Int]))
  print ("scanl1 singleton", scanl1 (+) [5 :: Int], scanl1 (undefined :: Int -> Int -> Int) [5])
  print ("scanl1 shape", scanl1 paren ["a", "b", "c"])
  print ("scanl1 infinite", take 5 (scanl1 (+) [1 :: Int ..]), take 4 (scanl1 (*) (repeat (2 :: Int))))
  print ("scanr", scanr (+) 0 [1, 2, 3, 4 :: Int])
  print ("scanr empty", scanr (+) 0 ([] :: [Int]), scanr (+) 7 ([] :: [Int]))
  print ("scanr singleton", scanr (+) 10 [5 :: Int])
  print ("scanr shape", scanr paren "z" ["a", "b", "c"])
  print ("scanr (-)", scanr (-) 100 [1, 2, 3 :: Int])
  print ("scanr different types", scanr (:) [] "abc", scanr (\c n -> n + fromEnum c - 96) 0 "abc")
  print ("scanr head = foldr", head (scanr (+) 0 [1 .. 10 :: Int]) == foldr (+) 0 [1 .. 10], last (scanr (+) 3 [1, 2 :: Int]))
  print ("scanr1", scanr1 (+) [1, 2, 3, 4 :: Int], scanr1 max [3, 1, 4, 1, 5, 9, 2, 6 :: Int])
  print ("scanr1 empty", scanr1 (+) ([] :: [Int]))
  print ("scanr1 singleton", scanr1 (+) [5 :: Int], scanr1 (undefined :: Int -> Int -> Int) [5])
  print ("scanr1 shape", scanr1 paren ["a", "b", "c"])
  print ("scanr1 (-)", scanr1 (-) [10, 1, 2, 3 :: Int])
  print ("fibs via scanl", take 10 (let fibs = 0 : scanl (+) 1 fibs in fibs :: [Integer]))
