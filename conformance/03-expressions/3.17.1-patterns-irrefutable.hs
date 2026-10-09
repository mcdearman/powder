-- Haskell 2010 Report 3.17.1 / 3.17.2: irrefutable patterns ~p always match; the value is examined only when a variable is used
module Main (main) where

lazyPair :: (Int, Int) -> Int
lazyPair ~(_, _) = 0

lazyUse :: Bool -> (Int, Int) -> Int
lazyUse useIt ~(a, b) = if useIt then a + b else -1

strictOuterLazyInner :: (Int, [Int]) -> Int
strictOuterLazyInner (n, ~(x : _)) = if n > 0 then x else n

-- the classic use: producing output before the argument is available
lazySplit :: [a] -> ([a], [a])
lazySplit [] = ([], [])
lazySplit (x : xs) = (x : odds, evens)
  where
    ~(evens, odds) = lazySplit xs

main :: IO ()
main = do
  print ("lazy on bottom", lazyPair undefined)
  print ("lazy unused", lazyUse False undefined)
  print ("lazy used", lazyUse True (3, 4))
  print ("inner lazy unused", strictOuterLazyInner (0, []), strictOuterLazyInner (-5, undefined))
  print ("inner lazy used", strictOuterLazyInner (1, [9, 8]))
  print ("lambda", (\ ~(a, b) -> 0) (undefined :: (Int, Int)))
  print ("lambda mismatch unused", (\ ~[x] -> 0) ([] :: [Int]))
  print ("lambda nested", (\ ~[x, ~(a, b)] -> x) [(0, 1), undefined])
  print ("lazy constructor mismatch", (\ ~(Just n) -> "never looked") (Nothing :: Maybe Int))
  print ("lazy literal mismatch", (\ ~1 -> "never compared") (2 :: Int))
  print ("case", case (undefined :: [Int]) of { ~(h : _) -> "matched" })
  print ("lazy always first", case Just 5 of { ~Nothing -> "lazy wins"; Just _ -> "unreachable" })
  print ("lazy on variable and wildcard", (\ ~x ~_ -> x) 'v' (undefined :: Int))
  print ("double tilde", (\ ~(~(a, b)) -> 1) (undefined :: (Int, Int)))
  print ("lazy as", (\ ~whole@(a, b) -> 2) (undefined :: (Int, Int)))
  print ("partial use", (\ ~(a, b) -> a) (5, undefined :: Int))
  print ("split", lazySplit [1, 2, 3, 4, 5])
  print ("split infinite", take 3 (fst (lazySplit [1 ..])))
  print ("lazy in comprehension", [0 | ~(Just _) <- [Nothing, Just 'a']])
  print ("lazy in do", do { ~(a, b) <- Just (1, 2); return (a + b) })
