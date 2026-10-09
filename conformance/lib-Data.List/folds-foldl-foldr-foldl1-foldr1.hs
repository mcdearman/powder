-- Haskell 2010 Report 20.3 (Data.List): reducing lists with foldl, foldl', foldl1, foldl1', foldr, foldr1
module Main (main) where

import Data.List

xs :: [Int]
xs = [1, 2, 3]

paren :: String -> String -> String
paren a b = "(" ++ a ++ "," ++ b ++ ")"

main :: IO ()
main = do
  print ("foldl minus", foldl (-) 10 xs)
  print ("foldr minus", foldr (-) 10 xs)
  print ("foldl shape", foldl paren "z" ["a", "b", "c"])
  print ("foldr shape", foldr paren "z" ["a", "b", "c"])
  print ("foldl empty", foldl (-) 10 [])
  print ("foldr empty", foldr (-) 10 [])
  print ("foldl reverse", foldl (flip (:)) [] xs)
  print ("foldr identity", foldr (:) [] xs)
  print ("foldl different types", foldl (\n s -> n + length s) 0 ["ab", "c", ""])
  print ("foldr different types", foldr (\c n -> n * 2 + fromEnum c - 48) 0 "101")
  print ("foldl does not force intermediate values", foldl (\_ x -> x) 0 [undefined, 5 :: Int])
  print ("foldr short circuits on infinite list", foldr (\x acc -> x > 5 || acc) False [1 :: Int ..])
  print ("foldr builds lazily", take 3 (foldr (\x acc -> x * 2 : acc) [] [1 :: Int ..]))
  print ("foldl' sum", foldl' (+) 0 [1 .. 100000 :: Int])
  print ("foldl' minus", foldl' (-) 10 xs)
  print ("foldl' shape", foldl' paren "z" ["a", "b", "c"])
  print ("foldl' empty", foldl' (+) 7 [])
  print ("foldl' agrees with foldl", foldl' (flip (:)) [] xs == foldl (flip (:)) [] xs)
  print ("foldl1 minus", foldl1 (-) [10, 2, 3 :: Int])
  print ("foldl1 shape", foldl1 paren ["a", "b", "c"])
  print ("foldl1 singleton", foldl1 paren ["a"])
  print ("foldl1 max", foldl1 max [3, 9, 2 :: Int])
  print ("foldl1' minus", foldl1' (-) [10, 2, 3 :: Int])
  print ("foldl1' shape", foldl1' paren ["a", "b", "c"])
  print ("foldl1' singleton", foldl1' paren ["a"])
  print ("foldl1' max", foldl1' max [3, 9, 2 :: Int])
  print ("foldr1 minus", foldr1 (-) [10, 2, 3 :: Int])
  print ("foldr1 shape", foldr1 paren ["a", "b", "c"])
  print ("foldr1 singleton", foldr1 paren ["a"])
  print ("foldr1 short circuits", foldr1 (\x acc -> x || acc) (map (> 2) [1 :: Int ..]))
  print ("foldl vs foldr on append", foldl (++) "" ["a", "b", "c"] == foldr (++) "" ["a", "b", "c"])
  print ("foldl1 is foldl from head", foldl1 (-) [10, 2, 3] == foldl (-) 10 [2, 3 :: Int])
