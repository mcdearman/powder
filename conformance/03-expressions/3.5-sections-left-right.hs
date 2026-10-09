-- Haskell 2010 Report 3.5: sections (op e) = \x -> x op e and (e op) = \x -> e op x
module Main (main) where

infixr 5 +++

(+++) :: [a] -> [a] -> [a]
xs +++ ys = foldr (:) ys xs

data P = Int :# Int
  deriving (Eq, Show)

main :: IO ()
main = do
  print ("(+ 1)", map (+ 1) [1, 2, 3])
  print ("(1 +)", map (1 +) [1, 2, 3])
  print ("(2 ^)", map (2 ^) [0, 1, 2, 3])
  print ("(^ 2)", map (^ 2) [0, 1, 2, 3])
  print ("(10 -)", map (10 -) [1, 2])
  print ("(/ 2)", map (/ 2) [1, 3])
  print ("(2 /)", map (2 /) [1, 4])
  print ("(: [])", map (: []) "abc")
  print ("(0 :)", (0 :) [1, 2])
  print ("(++ s)", (++ "!") "hey", ("<" ++) "hey")
  print ("(< 3)", filter (< 3) [1 .. 5], filter (3 <) [1 .. 5])
  print ("(== 3)", map (== 3) [2, 3], map (3 /=) [2, 3])
  print ("(. f)", ((* 2) . (+ 1)) 5, ((. (+ 1)) (* 2)) 5, (((* 2) .) (+ 1)) 5)
  print ("($ 3)", map ($ 3) [(+ 1), (* 2), subtract 1])
  print ("(f $)", (negate $) 3)
  print ("bool", (&& True) False, (True ||) False)
  print ("(!! 1)", (!! 1) "abc", ("abc" !!) 2)
  print ("user op", ("ab" +++) "cd", (+++ "ab") "cd")
  print ("conop", (1 :#) 2, (:# 1) 2)
  print ("identity right", (+ 1) 5 == (\x -> x + 1) 5, (`div` 2) 9 == (\x -> x `div` 2) 9)
  print ("identity left", (1 -) 5 == (\x -> 1 - x) 5, (9 `div`) 2 == (\x -> 9 `div` x) 2)
  print ("identity op", (+) 3 4 == (\x y -> x + y) 3 4)
  print ("nested", map (subtract 1 . (* 2)) [1, 2], (map (+ 1) .) (filter even) [1, 2, 3, 4])
  print ("no spaces", (+1) 2, (1+) 2, (*2) 3)
