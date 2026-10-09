-- List comprehensions, pattern guards, ranges with a step, lazy patterns,
-- and a pattern that fails in a do block.

pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(a, b, c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], a * a + b * b == c * c]

firsts :: [Maybe (a, b)] -> [a]
firsts ms = [x | Just (x, _) <- ms]

classify :: [(String, Int)] -> String -> String
classify table key
  | Just n <- lookup key table, even n = "even " ++ show n
  | Just n <- lookup key table = "odd " ++ show n
  | let k = length key, k > 3 = "long and missing"
  | otherwise = "missing"

lazily :: (Int, Int) -> Int
lazily ~(a, _) = 1

pairs :: [Int] -> Maybe Int
pairs xs = do
  [a, b] <- Just xs
  return (a + b)

main :: IO ()
main = do
  print (pythagorean 20)
  print (firsts [Just (1, 'a'), Nothing, Just (3, 'c')])
  let table = [("two", 2), ("three", 3)]
  putStrLn (unwords (map (classify table) ["two", "three", "four", "x"]))
  print ([1, 3 .. 11], [10, 8 .. 1], take 4 [0, 5 ..], ['a', 'c' .. 'i'])
  print (lazily undefined)
  print (pairs [1, 2], pairs [1, 2, 3])
  print [(x, y) | x <- [1 .. 3], let y = x * x, odd y]
