-- Haskell 2010 Report 3.13 / 3.17.2: pattern guards p <- e; qualifiers are tried in order and bind for the later ones
module Main (main) where

lookupBoth :: [(String, Int)] -> String -> String -> Maybe Int
lookupBoth env a b
  | Just x <- lookup a env, Just y <- lookup b env = Just (x + y)
  | Just x <- lookup a env = Just x
  | otherwise = Nothing

classify :: Maybe Int -> String
classify m
  | Just x <- m, x > 0 = "pos"
  | Just 0 <- m = "zero"
  | Just x <- m, let y = negate x, y > 10 = "very neg " ++ show y
  | Nothing <- m = "nothing"
classify _ = "neg (fell through to the next clause)"

firstWord :: String -> String
firstWord s
  | (w, _ : rest) <- break (== ' ') s, not (null w) = w ++ "|" ++ rest
  | otherwise = s

shadowing :: Int -> Int
shadowing x
  | Just x <- Just (x + 1), Just x <- Just (x * 10) = x
  | otherwise = 0

env0 :: [(String, Int)]
env0 = [("a", 1), ("b", 2)]

main :: IO ()
main = do
  print ("both", lookupBoth env0 "a" "b")
  print ("first only", lookupBoth env0 "a" "z")
  print ("second only", lookupBoth env0 "z" "b")
  print ("classify", map classify [Just 5, Just 0, Just (-20), Nothing, Just (-1)])
  print ("tuple pattern guard", firstWord "hello world", firstWord "single", firstWord " lead")
  print ("shadowing", shadowing 4)
  print ("in case", case [1, 2, 3] of { xs | (h : _) <- reverse xs, h > 2 -> h; _ -> 0 })
  print ("in case, falls through", case [3, 2, 1] of { xs | (h : _) <- reverse xs, h > 2 -> h; _ -> 0 })
  print ("in lambda case", map (\v -> case v of { n | Just k <- lookup n [(1, "one"), (2, "two")] -> k; _ -> "?" }) [1, 2, 3])
  print ("in pattern binding", let v | Just n <- lookup 'b' (zip "abc" [1 ..]) = n | otherwise = 0 in v)
  print ("irrefutable guard pattern", case 5 of { n | (a, b) <- (n, n * 2) -> a + b })
  print ("guard pattern failure skips bottom", case 1 of { n | Nothing <- Just n, undefined -> "a"; _ -> "skipped" })
  print ("mixed qualifiers", case (Just 3, [4, 5]) of { (m, l) | Just a <- m, even (length l), let s = sum l, (b : _) <- l -> a + s + b; _ -> 0 })
  print ("where visible in guards", viaWhere 3)
  where
    viaWhere :: Int -> String
    viaWhere k
      | Just s <- lookup k table = s
      | otherwise = "missing"
      where
        table = [(3, "three")]
