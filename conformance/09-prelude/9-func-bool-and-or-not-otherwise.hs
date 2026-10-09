-- Haskell 2010 Report 9: (&&), (||), not, otherwise; (&&) and (||) are lazy in their second argument
module Main (main) where

classify :: Int -> String
classify n
  | n < 0 = "negative"
  | n == 0 = "zero"
  | otherwise = "positive"

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

bools :: [Bool]
bools = [False, True]

main :: IO ()
main = do
  print ("&& table", [(a, b, a && b) | a <- bools, b <- bools])
  print ("|| table", [(a, b, a || b) | a <- bools, b <- bools])
  print ("not", not True, not False, not (not True))
  print ("otherwise", otherwise, otherwise == True)
  print ("guards", map classify [-5, 0, 5])
  print ("xor table", [xor a b | a <- bools, b <- bools])
  print ("&& short circuit", False && undefined, False && (undefined && True))
  print ("|| short circuit", True || undefined, True || (undefined || False))
  print ("&& needs first only when False", True && False, True && True)
  print ("guarding a partial function", null ([] :: [Int]) || head [] > (0 :: Int), not (null [1 :: Int]) && head [1] > (0 :: Int))
  print ("as functions", foldr (&&) True [True, False, undefined], foldr (||) False [False, True, undefined])
  print ("sections", map (&& True) bools, map (|| True) bools, map (False ||) bools)
  print ("zipWith", zipWith (&&) [True, True, False] [True, False, False], zipWith (||) [True, True, False] [True, False, False])
  print ("de Morgan", and [not (a && b) == (not a || not b) | a <- bools, b <- bools], and [not (a || b) == (not a && not b) | a <- bools, b <- bools])
  print ("if with not", if not (1 > (2 :: Int)) then "le" else "gt")
