-- Haskell 2010 Report 9 (and 4.4.2): infixr 5 : ++; infix 4 == /= < <= >= > elem notElem; infixr 3 &&; infixr 2 ||
-- Every expression below either changes value or fails to typecheck if a fixity is wrong.
module Main (main) where

main :: IO ()
main = do
  print (": is infixr", 1 : 2 : 3 : [] :: [Int])
  print ("++ is infixr 5 with :", 1 : [2] ++ [3] :: [Int], [1] ++ 2 : [3] :: [Int], [1] ++ [2] ++ 3 : [] :: [Int])
  print (": below +", 1 + 2 : [3 * 4] :: [Int])
  print ("++ below +", [1 + 1] ++ [2 * 2 - 1] :: [Int])
  print ("++ associativity is lazy-friendly", take 3 ([1] ++ [2] ++ [3 :: Int ..]))
  print ("== below +", 1 + 2 == (3 :: Int), 2 * 3 /= 3 + (3 :: Int))
  print ("< below + and *", 1 + 1 < 2 * (2 :: Int), 10 - 1 >= 3 ^ (2 :: Int), 7 `div` 2 <= (3 :: Int), 2 ^ (3 :: Int) > (7 :: Int))
  print ("== below ++ and :", [1] ++ [2] == 1 : [2 :: Int], "ab" ++ "c" < "ab" ++ "d")
  print ("elem is infix 4: below :", (3 :: Int) `elem` 1 : 2 : [3])
  print ("elem is infix 4: below ++", 'c' `elem` "ab" ++ "c", 'c' `notElem` "ab" ++ "c")
  print ("elem is infix 4: below +", 1 + 2 `elem` [3 :: Int], 2 * 2 `notElem` [3 :: Int])
  print ("elem above &&", 'a' `elem` "abc" && 'z' `notElem` "abc")
  print ("&& below comparisons", 1 < (2 :: Int) && 'a' /= 'b', 1 == (1 :: Int) && 2 >= (3 :: Int))
  print ("&& above ||", True || True && False, (True || True) && False)
  print ("&& above || (left)", False && False || True, False && (False || True))
  print ("|| is infixr: skips the rest", True || undefined || undefined)
  print ("&& is infixr: skips the rest", False && undefined && undefined)
  print ("|| below comparisons", 1 > (2 :: Int) || 3 <= (3 :: Int))
  print ("not is ordinary application", not True || True, not (True || True))
  print ("mixed", 1 + 1 == (2 :: Int) && 3 `elem` [1, 2 :: Int] || 'x' : "y" == "x" ++ "y")
