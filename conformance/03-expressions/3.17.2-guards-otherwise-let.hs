-- Haskell 2010 Report 3.13 / 3.17.2: boolean guards, otherwise, let in guards, and fall-through between function clauses
module Main (main) where

grade :: Int -> Char
grade n
  | n >= 90 = 'A'
  | n >= 80 = 'B'
  | n >= 70 = 'C'
  | otherwise = 'F'

bmi :: Int -> Int -> String
bmi w h
  | let v = w `div` (h * h), v < 2 = "low " ++ show v
  | let v = w `div` (h * h), v < 4 = "mid " ++ show v
  | otherwise = "high"

-- when every guard of a clause fails, the next clause is tried
fall :: Int -> Int -> String
fall x y
  | x > 10 = "x big"
  | y > 10 = "y big"
fall 0 _ = "x zero"
fall _ y
  | even y = "y even"
fall _ _ = "last"

andGuard :: Int -> String
andGuard n
  | n > 0, even n = "positive even"
  | n > 0 = "positive odd"
  | True = "non-positive"

letScope :: Int -> Int
letScope n
  | let a = n + 1, let b = a * 2, b > 10 = a + b
  | let { a = n - 1 } = a

main :: IO ()
main = do
  print ("grade", map grade [95, 85, 75, 10])
  print ("first true guard wins", grade 100)
  print ("let in guards", bmi 30 5, bmi 80 5, bmi 200 5)
  print ("fall through", fall 11 0, fall 0 11, fall 0 0, fall 1 2, fall 1 3)
  print ("comma is conjunction", map andGuard [2, 3, 0])
  print ("let scopes over later qualifiers and body", letScope 5, letScope 1)
  print ("otherwise", otherwise, otherwise == True)
  print ("guard in lambda via case", (\n -> case () of { _ | n < 0 -> "neg" | n == 0 -> "zero" | otherwise -> "pos" }) 0)
  print ("guard in local function", let { sgn k | k < 0 = -1 | k > 0 = 1 | otherwise = 0 } in map sgn [-7, 0, 7])
  print ("guard evaluation is lazy", let { pick b | b = "taken" | undefined = "never" } in pick True)
  print ("guards on operator definition", 3 <=> 5, 5 <=> 3, 4 <=> 4)
  where
    (<=>) :: Int -> Int -> Ordering
    a <=> b
      | a < b = LT
      | a > b = GT
      | otherwise = EQ
