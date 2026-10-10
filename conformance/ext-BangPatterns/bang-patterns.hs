{-# LANGUAGE BangPatterns #-}
-- BangPatterns: !pattern works out what it is matched against before matching
module Main (main) where

sumTo :: Int -> Int -> Int
sumTo !acc 0 = acc
sumTo !acc n = sumTo (acc + n) (n - 1)

first :: a -> b -> a
first x !_ = x

lazyFirst :: a -> b -> a
lazyFirst x _ = x

mean :: [Double] -> Double
mean = go 0 0
  where
    go !s !n [] = s / fromIntegral (n :: Int)
    go !s !n (v : vs) = go (s + v) (n + 1) vs

strictPair :: (Int, Int) -> Int
strictPair (!a, !b) = a + b

-- an operator called ! is still an operator where it has something on both sides
(!) :: [a] -> Int -> a
xs ! n = xs !! n

main :: IO ()
main = do
  print (sumTo 0 100000)
  print (lazyFirst 'a' (undefined :: Int))
  print (first 'b' (5 :: Int))
  print (mean [1, 2, 3, 4])
  print (strictPair (3, 4))
  print ((\ !v -> v + 1) (2 :: Int))
  print (case Just (6 :: Int) of Just !v -> v; Nothing -> 0)
  print ("abc" ! 1, [10, 20, 30 :: Int] ! 2)
  print (let f !_ = 'k' in f ())
