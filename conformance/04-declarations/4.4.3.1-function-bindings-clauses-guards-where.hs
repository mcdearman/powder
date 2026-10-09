-- Haskell 2010 Report 4.4.3.1: function bindings with several clauses, guards, and where scoping over all guards of a clause
module Main (main) where

classify :: Int -> String
classify 0 = "zero"
classify n
  | n < 0 = "negative"
  | even n = "even"
  | isBig = "big odd"
  | otherwise = "small odd"
  where
    isBig = n > limit
    limit = 100

-- when every guard of a clause fails, matching falls through to the next clause
fall :: Int -> Int -> String
fall x y | x > y = "gt"
fall x y | x == y = "eq"
fall _ _ = "lt"

-- each clause has its own where; the same local name may be reused
area :: String -> Int -> Int
area "square" n = side * side
  where side = n
area "double" n | n > 0 = side
                | otherwise = 0
  where side = 2 * n
area _ _ = -1

-- clauses are tried top to bottom
firstMatch :: (Int, Bool) -> String
firstMatch (0, _) = "first zero"
firstMatch (_, True) = "second true"
firstMatch (1, False) = "one false"
firstMatch _ = "other"

-- pattern guards and guards with let (Haskell 2010)
lookupOr :: String -> [(String, Int)] -> Int
lookupOr k env
  | Just v <- lookup k env, v > 0 = v
  | let { n = length k }, n > 3 = n
  | otherwise = 0

-- where bindings see the arguments and each other, and may be functions
collatz :: Int -> Int
collatz n = go n 0
  where
    go 1 acc = acc
    go m acc
      | even m = go (m `div` 2) next
      | otherwise = go (3 * m + 1) next
      where
        next = acc + 1

ack :: Int -> Int -> Int
ack 0 n = n + 1
ack m 0 = ack (m - 1) 1
ack m n = ack (m - 1) (ack m (n - 1))

main :: IO ()
main = do
  print ("classify", map classify [0, -5, 4, 101, 7])
  print ("fall", fall 2 1, fall 1 1, fall 0 1)
  print ("area", area "square" 4, area "double" 4, area "double" (-4), area "other" 1)
  print ("firstMatch", map firstMatch [(0, True), (2, True), (1, False), (2, False)])
  print ("lookupOr", lookupOr "a" [("a", 5)], lookupOr "a" [("a", -5)], lookupOr "long key" [], lookupOr "k" [])
  print ("collatz", map collatz [1, 2, 6, 27])
  print ("ack", ack 2 3)
