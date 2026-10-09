-- Haskell 2010 Report 9 (PreludeList): elem, notElem, lookup (first match wins on duplicate keys)
module Main (main) where

table :: [(Int, String)]
table = [(1, "one"), (2, "two"), (3, "three"), (2, "second two"), (1, "second one")]

-- an Eq instance that is not structural, to show elem/lookup use (==)
newtype CI = CI Char

instance Eq CI where
  CI a == CI b = lower a == lower b
    where
      lower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

main :: IO ()
main = do
  print ("elem", 3 `elem` [1, 2, 3 :: Int], 4 `elem` [1, 2, 3 :: Int], 'a' `elem` "banana", elem "b" ["a", "b"])
  print ("elem empty", 1 `elem` ([] :: [Int]), 'x' `elem` "", (undefined :: Int) `elem` [])
  print ("elem first/last/only", 1 `elem` [1, 2, 3 :: Int], 3 `elem` [1, 2, 3 :: Int], () `elem` [()])
  print ("elem stops at first hit", 2 `elem` [1, 2, undefined :: Int])
  print ("elem on structures", (1, 'a') `elem` [(1 :: Int, 'b'), (1, 'a')], Just 2 `elem` [Nothing, Just (2 :: Int)], [1] `elem` [[], [1 :: Int]])
  print ("elem on doubles", 0.5 `elem` [0.25, 0.5 :: Double], (0 / 0 :: Double) `elem` [0 / 0])
  print ("notElem", 3 `notElem` [1, 2, 3 :: Int], 4 `notElem` [1, 2, 3 :: Int], 'z' `notElem` "banana")
  print ("notElem empty", 1 `notElem` ([] :: [Int]), 'x' `notElem` "", (undefined :: Int) `notElem` [])
  print ("notElem = not . elem", all (\x -> notElem x "abc" == not (elem x "abc")) "abcxyz")
  print ("elem uses ==", CI 'a' `elem` [CI 'X', CI 'A'], CI 'q' `notElem` [CI 'X', CI 'A'])
  print ("elem sections", filter (`elem` "aeiou") "education", filter (`notElem` "aeiou") "education", map (elem 'a') ["cat", "dog"])
  print ("lookup hit", lookup 1 table, lookup 3 table)
  print ("lookup miss", lookup 4 table, lookup 0 table, lookup (-1) table)
  print ("lookup duplicate keys: first wins", lookup 2 table, lookup 2 (reverse table))
  print ("lookup empty", lookup 'a' ([] :: [(Char, Int)]), lookup (undefined :: Int) ([] :: [(Int, Int)]))
  print ("lookup string keys", lookup "b" [("a", 1), ("b", 2 :: Int)], lookup "" [("", 'e')], lookup "B" [("b", 'x')])
  print ("lookup value not forced", fmap (const 'v') (lookup 2 [(1, undefined), (2, undefined :: Int)]))
  print ("lookup later entries not forced", lookup 'a' (('a', 1 :: Int) : undefined))
  print ("lookup structured keys", lookup (1, 'x') [((1 :: Int, 'y'), "no"), ((1, 'x'), "yes")], lookup [2] [([1 :: Int], 'a'), ([2], 'b')])
  print ("lookup with zip", lookup 'c' (zip "abc" [1 :: Int ..]), lookup 'd' (zip "abc" [1 :: Int ..]))
  print ("lookup then maybe", maybe "?" id (lookup 2 table), maybe "?" id (lookup 9 table))
  print ("lookup uses ==", fmap (const True) (lookup (CI 'a') [(CI 'B', ()), (CI 'A', ())]))
  print ("lookup all keys", map (`lookup` table) [1, 2, 3, 4])
