-- Haskell 2010 Report 3.14: do expressions in the list monad
module Main (main) where

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

triples :: Int -> [(Int, Int, Int)]
triples n = do
  a <- [1 .. n]
  b <- [a .. n]
  c <- [b .. n]
  if a * a + b * b == c * c then return (a, b, c) else []

evens :: [Int] -> [Int]
evens xs = do
  x <- xs
  let y = x * 2
      z = y + 1
  [y, z]

main :: IO ()
main = do
  print ("pairs", pairs [1, 2, 3] "ab")
  print ("empty source", pairs ([] :: [Int]) "ab", pairs [1, 2] "")
  print ("triples", triples 20)
  print ("let and final list", evens [1, 2])
  print ("then repeats", do { [(), ()]; "xy" })
  print ("then empty", do { [] :: [Int]; "xy" })
  print ("wildcard bind", do { _ <- [1, 2, 3]; return 'k' })
  print ("guard by empty list", do { x <- [1 .. 6]; if even x then [()] else []; return x })
  print ("dependent", do { x <- [1, 2, 3]; y <- replicate x x; return y })
  print ("tuple pattern", do { (a, b) <- zip [1, 2, 3] "abc"; return (b, a) })
  print ("last is expression", do { x <- [1, 2]; [x, x * 10] })
  print ("return then bind", do { x <- return 5; y <- [x, x + 1]; return (y * 2) })
  print ("concat", do { xs <- [[1, 2], [], [3]]; xs })
  print ("three levels", length (do { a <- "ab"; b <- "cde"; c <- "fghi"; return [a, b, c] }))
  print ("mapM", mapM (\x -> [x, x + 10]) [1, 2])
  print ("sequence", sequence [[1, 2], [3], [4, 5]])
