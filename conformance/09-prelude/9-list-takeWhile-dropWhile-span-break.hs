-- Haskell 2010 Report 9 (PreludeList): takeWhile, dropWhile, span, break
module Main (main) where

xs :: [Int]
xs = [1, 2, 3, 4, 1, 2]

main :: IO ()
main = do
  print ("takeWhile", takeWhile (< 3) xs, takeWhile even xs, takeWhile (< 10) xs)
  print ("takeWhile stops at first failure", takeWhile odd [1, 3, 4, 5, 7 :: Int])
  print ("takeWhile empty", takeWhile (< 3) ([] :: [Int]), takeWhile undefined ([] :: [Int]))
  print ("takeWhile infinite", takeWhile (< 5) [1 :: Int ..], takeWhile (/= 'c') (cycle "abc"))
  print ("takeWhile lazy", takeWhile (< 3) (1 : 2 : 3 : undefined) :: [Int])
  print ("dropWhile", dropWhile (< 3) xs, dropWhile even xs, dropWhile (< 10) xs)
  print ("dropWhile drops only the prefix", dropWhile odd [1, 3, 4, 5, 7 :: Int])
  print ("dropWhile empty", dropWhile (< 3) ([] :: [Int]), dropWhile undefined ([] :: [Int]))
  print ("dropWhile infinite", take 3 (dropWhile (< 5) [1 :: Int ..]))
  print ("dropWhile spaces", dropWhile (== ' ') "   indented text", dropWhile (== ' ') "")
  print ("span", span (< 3) xs, span even xs, span (< 10) xs)
  print ("span empty", span (< 3) ([] :: [Int]), span undefined ([] :: [Int]))
  print ("span string", span (/= ' ') "hello world", span (== 'a') "aaab", span (== 'a') "baaa")
  print ("span infinite", fst (span (< 4) [1 :: Int ..]), take 2 (snd (span (< 4) [1 :: Int ..])))
  print ("span law", span even xs == (takeWhile even xs, dropWhile even xs), span (< 3) xs == (takeWhile (< 3) xs, dropWhile (< 3) xs))
  print ("break", break (> 3) xs, break even xs, break (> 10) xs, break (< 10) xs)
  print ("break empty", break (> 3) ([] :: [Int]), break undefined ([] :: [Int]))
  print ("break string", break (== ' ') "hello world", break (== ',') "no comma", break (== ',') ",leading")
  print ("break infinite", fst (break (> 3) [1 :: Int ..]), take 2 (snd (break (> 3) [1 :: Int ..])))
  print ("break = span . not", break even xs == span (not . even) xs, break (> 3) xs == span (<= 3) xs)
  print ("concat of parts", (\(a, b) -> a ++ b) (span odd xs) == xs, (\(a, b) -> a ++ b) (break odd xs) == xs)
  print ("predicate called until first failure only", takeWhile (\x -> x < 3 || undefined) [1, 2 :: Int] == [1, 2], fst (span (< 2) [1, 2, undefined :: Int]))
  print ("nested", span null [[], [], [1 :: Int], []], break null ["a", "", "b"])
