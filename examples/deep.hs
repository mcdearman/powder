-- Going deep: a sum that is not a loop, a list a million long made and
-- let go of, and a chain of thunks as long. None of it is deeper than the
-- machine's own stack of frames can grow.
total :: [Int] -> Int
total [] = 0
total (x : xs) = x + total xs

count :: Int -> Int
count 0 = 0
count n = 1 + count (n - 1)

main :: IO ()
main = do
  print (total [1 .. 1000000])
  print (count 1000000)
  print (length (map (+ 1) [1 .. 1000000 :: Int]))
  print (foldr (\x acc -> x + acc) 0 [1 .. 1000000 :: Int])
  print (foldl (+) 0 [1 .. 1000000 :: Int])
