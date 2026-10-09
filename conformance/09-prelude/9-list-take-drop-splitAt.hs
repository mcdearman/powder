-- Haskell 2010 Report 9 (PreludeList): take, drop, splitAt with zero, negative and too-large counts
module Main (main) where

xs :: [Int]
xs = [1, 2, 3, 4, 5]

counts :: [Int]
counts = [-3, -1, 0, 1, 2, 4, 5, 6, 100]

main :: IO ()
main = do
  mapM_ (\n -> print ("take/drop/splitAt", n, take n xs, drop n xs, splitAt n xs)) counts
  print ("law", all (\n -> splitAt n xs == (take n xs, drop n xs)) counts, all (\n -> take n xs ++ drop n xs == xs) counts)
  print ("empty list", take 3 ([] :: [Int]), drop 3 ([] :: [Int]), splitAt 3 ([] :: [Int]))
  print ("empty list zero/negative", take 0 ([] :: [Int]), drop (-1) ([] :: [Int]), splitAt 0 "")
  print ("extreme counts", take minBound xs, drop minBound xs, take maxBound xs, drop maxBound xs)
  print ("splitAt extreme", splitAt minBound "ab", splitAt maxBound "ab")
  print ("strings", take 3 "hello", drop 3 "hello", splitAt 3 "hello")
  print ("take infinite", take 5 [1 :: Int ..], take 3 (repeat 'x'), take 4 (cycle [True, False]))
  print ("drop infinite", take 3 (drop 1000 [1 :: Int ..]), head (drop 5 (iterate (* 2) (1 :: Int))))
  print ("splitAt infinite", fst (splitAt 3 [1 :: Int ..]), take 2 (snd (splitAt 3 [1 :: Int ..])))
  print ("take 0 does not force list", take 0 (undefined :: [Int]), take (-1) (undefined :: [Int]))
  print ("take n lazy in tail", take 2 (1 : 2 : undefined) :: [Int])
  print ("take lazy in elements", length (take 2 [undefined, undefined, undefined :: Int]))
  print ("drop lazy in dropped elements", drop 2 [undefined, undefined, 3 :: Int])
  print ("splitAt lazy", fst (splitAt 1 ('a' : undefined)), length (snd (splitAt 1 [undefined, 'b'])))
  print ("take of take", take 2 (take 4 xs), take 4 (take 2 xs), drop 1 (drop 2 xs), drop 2 (take 4 xs))
  print ("take then drop", take 2 (drop 1 xs), drop 1 (take 3 xs))
  print ("lengths", length (take 3 xs), length (drop 3 xs), length (take 1000 [1 :: Int ..]))
  print ("nested lists", take 1 [[1, 2], [3 :: Int]], splitAt 1 ["ab", "cd", "ef"])
