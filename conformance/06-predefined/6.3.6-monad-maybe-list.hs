-- Haskell 2010 Report 6.3.6: class Monad; (>>=), (>>), return at Maybe and lists; mapM, sequence, (=<<)
half :: Int -> Maybe Int
half n
  | even n = Just (n `div` 2)
  | otherwise = Nothing

neighbours :: Int -> [Int]
neighbours n = [n - 1, n + 1]

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

main :: IO ()
main = do
  print ("Maybe bind", Just 8 >>= half, Just 7 >>= half, Nothing >>= half)
  print ("Maybe bind chain", Just 8 >>= half >>= half, Just 6 >>= half >>= half, Just 8 >>= half >>= half >>= half >>= half)
  print ("Maybe then", Just 'a' >> Just 'b', (Nothing :: Maybe Int) >> Just 'b', Just 'a' >> (Nothing :: Maybe Int))
  print ("Maybe return", return 3 :: Maybe Int, (return 8 >>= half) == half 8)
  print ("Maybe short circuits", (Nothing :: Maybe Int) >>= \_ -> (undefined :: Maybe Int))
  print ("list bind", [1, 2, 3] >>= neighbours, [] >>= neighbours, [10] >>= neighbours >>= neighbours)
  print ("list bind to empty", [1, 2, 3] >>= \x -> if odd x then [x] else [])
  print ("list then", [1, 2 :: Int] >> "ab", ([] :: [Int]) >> "ab", [1, 2 :: Int] >> "")
  print ("list return", return 'x' :: String, return 1 :: [Int])
  print ("list do", pairs [1, 2 :: Int] "ab", pairs ([] :: [Int]) "ab")
  print ("list do with let and filter", do { x <- [1 .. 4 :: Int]; let { y = x * x }; if even y then [y] else [] })
  print ("=<<", half =<< Just 10, neighbours =<< [0, 10], (half =<<) (Just 3))
  print ("mapM Maybe", mapM half [2, 4, 6], mapM half [2, 3, 6], mapM half [])
  print ("mapM list", mapM (\x -> [x, x * 10]) [1, 2 :: Int])
  print ("sequence Maybe", sequence [Just 1, Just 2 :: Maybe Int], sequence [Just 1, Nothing :: Maybe Int])
  print ("sequence list", sequence [[1, 2], [3], [4, 5 :: Int]], sequence [[1, 2], [], [3 :: Int]])
  print ("sequence empty", sequence ([] :: [Maybe Int]), sequence ([] :: [[Int]]))
  print ("mapM_/sequence_ Maybe", mapM_ half [2, 4], mapM_ half [2, 3], sequence_ [Just 'a', Nothing])
  print ("precedence: >>= and >> are infixl 1", Just 1 >> Just 2 >>= \x -> Just (x + 1 :: Int))
  print ("=<< is infixr 1", half =<< half =<< Just 12)
