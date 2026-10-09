merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort a) (msort b)
  where
    (a, b) = splitAt (length xs `div` 2) xs

-- A linear congruential generator, for something to sort.
randoms :: Int -> [Int]
randoms = map (`mod` 1000) . tail . iterate (\x -> (x * 1103515245 + 12345) `mod` 2147483648)

main :: IO ()
main = do
  let xs = take 200 (randoms 42)
      sorted = msort xs
  putStrLn (unwords (map show (take 10 sorted)))
  print (sum sorted)
  putStrLn (if and (zipWith (<=) sorted (tail sorted)) then "sorted" else "not sorted")
