-- Haskell 2010 Report 3.11: two generators and a guard keeping the diagonal, consumed by a recursive function
module Main (main) where

diagonal :: [Int]
diagonal = [x + y | x <- [0 .. 10], y <- [0 .. 10], x == y]

total :: [Int] -> Int
total [] = 0
total (x : xs) = x + total xs

main :: IO ()
main = do
  print ("diagonal", diagonal)
  print ("total", total diagonal)
  print ("off diagonal count", length [() | x <- [0 .. 10 :: Int], y <- [0 .. 10 :: Int], x /= y])
  print ("guard between generators", [(x, y) | x <- [1 .. 4 :: Int], even x, y <- "ab"])
  print ("later generator depends on earlier", total [y | x <- [1 .. 4], y <- [1 .. x]])
