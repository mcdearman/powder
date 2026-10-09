-- Haskell 2010 Report 20.4.3, 20.4.4 (Data.List): infinite lists iterate, repeat, replicate, cycle, and
-- unfolding with unfoldr
module Main (main) where

import Data.List

collatz :: Int -> [Int]
collatz = unfoldr step
  where
    step 0 = Nothing
    step 1 = Just (1, 0)
    step n = Just (n, if even n then n `div` 2 else 3 * n + 1)

bits :: Int -> [Int]
bits = unfoldr (\n -> if n == 0 then Nothing else Just (n `mod` 2, n `div` 2))

main :: IO ()
main = do
  print ("iterate", take 6 (iterate (* 2) (1 :: Int)))
  print ("iterate first is seed", head (iterate (+ 1) (10 :: Int)))
  print ("iterate never applies function for the head", take 1 (iterate (\_ -> undefined) (5 :: Int)))
  print ("iterate strings", take 4 (iterate ('a' :) ""))
  print ("iterate index", iterate (`div` 2) (1024 :: Int) !! 10)
  print ("iterate until", takeWhile (< 100) (iterate (\x -> x * x + 1) (1 :: Int)))
  print ("repeat", take 4 (repeat 'x'))
  print ("repeat zip", zip "abc" (repeat (0 :: Int)))
  print ("replicate", replicate 3 'x')
  print ("replicate zero", replicate 0 'x')
  print ("replicate negative", replicate (-2) 'x')
  print ("replicate nested", replicate 2 (replicate 2 (0 :: Int)))
  print ("replicate does not force element", length (replicate 4 (undefined :: Int)))
  print ("replicate is take of repeat", replicate 5 True == take 5 (repeat True))
  print ("cycle", take 7 (cycle [1, 2, 3 :: Int]))
  print ("cycle singleton", take 3 (cycle "a"))
  print ("cycle of infinite", take 4 (cycle [1 :: Int ..]))
  print ("cycle index", cycle "abc" !! 100)
  print ("cycle drop", take 3 (drop 299 (cycle "abc")))
  print ("unfoldr countdown", unfoldr (\n -> if n == 0 then Nothing else Just (n, n - 1)) (5 :: Int))
  print ("unfoldr immediately Nothing", unfoldr (\_ -> Nothing :: Maybe (Char, Int)) 0)
  print ("unfoldr squares", unfoldr (\n -> if n > 5 then Nothing else Just (n * n, n + 1)) (1 :: Int))
  print ("unfoldr infinite", take 5 (unfoldr (\(a, b) -> Just (a, (b, a + b))) (0 :: Integer, 1)))
  print ("unfoldr different seed and element types", unfoldr (\s -> if null s then Nothing else Just (length s, tail s)) "abc")
  print ("unfoldr bits", bits 10, bits 255, bits 0)
  print ("unfoldr collatz", collatz 6)
  print ("unfoldr chunks", unfoldr (\s -> if null s then Nothing else Just (splitAt 3 s)) "abcdefgh")
  print ("unfoldr undoes foldr", unfoldr (\s -> case s of { [] -> Nothing; (c : cs) -> Just (c, cs) }) "same" == foldr (:) [] "same")
