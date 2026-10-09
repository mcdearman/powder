-- Haskell 2010 Report 6.1.4: tuples; fst, snd, curry, uncurry and the zip family
swap2 :: (a, b) -> (b, a)
swap2 (x, y) = (y, x)

addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

main :: IO ()
main = do
  print ("fst/snd", fst (1 :: Int, 'a'), snd (1 :: Int, 'a'))
  print ("nested", fst (snd ('x', (True, "s"))), snd (fst ((1 :: Int, 'c'), ())))
  print ("constructor functions", (,) 'a' True, (,,) 'a' True (), (,,,) 1 2 3 (4 :: Int))
  print ("partial constructor", map ((,) 'k') [1, 2 :: Int])
  print ("curry", curry addPair 3 4, curry fst 'a' 'b', curry snd 'a' 'b')
  print ("uncurry", uncurry (+) (3, 4 :: Int), uncurry max ('a', 'z'), uncurry replicate (3, 'x'))
  print ("curry . uncurry", curry (uncurry (-)) 10 (3 :: Int), uncurry (curry addPair) (5, 6))
  print ("lazy components", fst ('a', undefined :: Int), snd (undefined :: Int, 'b'))
  print ("lazy pattern", (\ ~(_, _) -> 'k') (undefined :: (Int, Int)))
  print ("swap", swap2 (1 :: Int, "one"), swap2 (swap2 ('p', 'q')))
  print ("zip", zip [1, 2, 3 :: Int] "ab", zip "abc" [True, False])
  print ("zip empty", zip ([] :: [Int]) "abc", zip "abc" ([] :: [Int]))
  print ("zip lazy in second when first empty", zip ([] :: [Int]) (undefined :: [Char]))
  print ("zip infinite", zip [(1 :: Int) ..] "xyz")
  print ("zip3", zip3 [1, 2 :: Int] "ab" [True, False, True])
  print ("zipWith", zipWith (+) [1, 2, 3] [10, 20 :: Int], zipWith (,) "ab" "cd")
  print ("zipWith3", zipWith3 (\a b c -> a + b * c) [1, 2] [3, 4] [5, 6 :: Int])
  print ("unzip", unzip [(1 :: Int, 'a'), (2, 'b')], unzip ([] :: [(Int, Char)]))
  print ("unzip3", unzip3 [(1 :: Int, 'a', True), (2, 'b', False)])
  print ("unzip . zip", unzip (zip [1, 2, 3 :: Int] "abc"))
