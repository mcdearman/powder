-- Haskell 2010 Report 6.3.6 / 3.14: fail; a failing pattern in a do block calls fail (Nothing in Maybe, [] in lists)
justs :: [Maybe Int] -> [Int]
justs ms = do
  Just x <- ms
  return x

firstOfNonEmpty :: Maybe [Int] -> Maybe Int
firstOfNonEmpty m = do
  (x : _) <- m
  return x

pairsWithTrue :: [(Bool, Char)] -> String
pairsWithTrue ps = do
  (True, c) <- ps
  return c

lefts' :: [Either Int Char] -> [Int]
lefts' es = do
  Left n <- es
  [n, n * 10]

both :: Maybe (Maybe Int) -> Maybe Int
both mm = do
  Just x <- mm
  Just (x + 1)

literal :: [Int] -> [String]
literal ns = do
  1 <- ns
  return "one"

main :: IO ()
main = do
  print ("explicit fail Maybe", fail "ignored message" :: Maybe Int)
  print ("explicit fail list", fail "ignored message" :: [Int])
  print ("fail in a Maybe chain", Just 1 >> (fail "x" :: Maybe Int), (fail "x" :: Maybe Int) >> Just 2)
  print ("fail in a list chain", [1, 2 :: Int] >>= \x -> if x == 1 then fail "x" else [x])
  print ("list pattern failure skips", justs [Just 1, Nothing, Just 3], justs [Nothing], justs [])
  print ("Maybe pattern failure", firstOfNonEmpty (Just [7, 8]), firstOfNonEmpty (Just []), firstOfNonEmpty Nothing)
  print ("tuple with literal pattern", pairsWithTrue [(True, 'a'), (False, 'b'), (True, 'c')])
  print ("Either pattern", lefts' [Left 1, Right 'x', Left 2])
  print ("nested Maybe", both (Just (Just 1)), both (Just Nothing), both Nothing)
  print ("numeric literal pattern", literal [1, 2, 1, 3], literal [2])
  print ("irrefutable patterns never fail", do { (a, b) <- Just (1 :: Int, 'q'); return (b, a) })
  print ("variable pattern never fails", do { v <- [Nothing, Just 'j']; return v })
  print ("lazy pattern does not fail", do { ~(Just _) <- [Nothing, Just 'j']; return 'k' })
  print ("comprehension skips too", [x | Just x <- [Just 'a', Nothing, Just 'b']])
