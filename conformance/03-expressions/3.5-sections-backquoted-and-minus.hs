-- Haskell 2010 Report 3.5: sections of backquoted identifiers; (- e) is not a section, use subtract
module Main (main) where

data Shape = Rect Int Int
  deriving (Eq, Show)

plus3 :: Int -> Int -> Int -> Int
plus3 a b c = a * 100 + b * 10 + c

main :: IO ()
main = do
  print ("(`div` 2)", map (`div` 2) [7, 8, 9])
  print ("(7 `div`)", map (7 `div`) [1, 2, 3])
  print ("(`mod` 3)", map (`mod` 3) [3, 4, 5])
  print ("(`elem` s)", filter (`elem` "aeiou") "education")
  print ("(c `elem`)", map ('a' `elem`) ["abc", "xyz"])
  print ("(`max` 5)", map (`max` 5) [3, 7])
  print ("(`compare` 3)", map (`compare` 3) [2, 3, 4])
  print ("(`replicate` c)", (`replicate` 'x') 3, (3 `replicate`) 'y')
  print ("(`plus3` 2)", (`plus3` 2) 1 3, (1 `plus3`) 2 3)
  print ("constructor", (`Rect` 3) 4, (4 `Rect`) 3)
  print ("(`subtract` 1)", (`subtract` 1) 10, (10 `subtract`) 1)
  print ("(- 5) is a value", (- 5) + 0, abs (- 5))
  print ("(subtract 1)", map (subtract 1) [1, 2, 3])
  print ("(+ (-1))", map (+ (-1)) [1, 2, 3])
  print ("flip (-)", map (flip (-) 1) [1, 2, 3])
  print ("lambda", map (\x -> x - 1) [1, 2, 3])
  print ("(5 -)", map (5 -) [1, 2, 3])
  print ("(-) prefix", (-) 5 1, foldl (-) 10 [1, 2])
  print ("all agree", map (subtract 2) [5, 6] == map (\x -> x - 2) [5, 6])
