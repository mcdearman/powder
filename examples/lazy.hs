-- Infinite and self-referential data.

primes :: [Int]
primes = sieve [2 ..]
  where
    sieve (p : xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

collatz :: Int -> [Int]
collatz = takeWhile (/= 1) . iterate step
  where
    step n
      | even n = n `div` 2
      | otherwise = 3 * n + 1

main :: IO ()
main = do
  putStrLn (unwords (map show (take 20 primes)))
  print (fibs !! 90)
  print (length (collatz 27))
