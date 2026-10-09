-- Haskell 2010 Report 3.5: (e op) is legal iff (e op x) parses like ((e) op x); (op e) iff (x op e) parses like (x op (e))
module Main (main) where

infixl 6 |-|
infixr 5 |:|

(|-|) :: Int -> Int -> Int
a |-| b = a - b

(|:|) :: Int -> Int -> Int
a |:| b = a - b

sub :: Int -> Int -> Int
sub a b = a - b

main :: IO ()
main = do
  -- left sections whose operand uses the same left-associative operator
  print ("(1 + 2 +)", (1 + 2 +) 3)
  print ("(10 - 2 -)", (10 - 2 -) 3)
  print ("(10 |-| 2 |-|)", (10 |-| 2 |-|) 3)
  print ("(10 `sub` 3 `sub`)", (10 `sub` 3 `sub`) 2)
  -- right sections whose operand uses the same right-associative operator
  print ("(++ b ++ c)", (++ "b" ++ "c") "a")
  print ("(: 2 : [])", (: 2 : []) 1)
  print ("(^ 3 ^ 2)", (^ 3 ^ 2) 2)
  print ("(|:| 2 |:| 3)", (|:| 2 |:| 3) 1)
  print ("(. f . g)", (. (+ 1) . (* 2)) show 5)
  -- operand with tighter-binding operators
  print ("(+ 2 * 3)", (+ 2 * 3) 1)
  print ("(2 * 3 +)", (2 * 3 +) 1)
  print ("(== 1 + 1)", (== 1 + 1) 2)
  print ("(1 + 1 ==)", (1 + 1 ==) 2)
  print ("(|| a && b)", (|| True && False) False)
  print ("(a && b ||)", (True && False ||) False)
  print ("(: xs ++ ys)", (: [2] ++ [3]) 1)
  print ("(2 ^ 2 *)", (2 ^ 2 *) 5, (* 2 ^ 2) 5)
  -- function application binds tighter than any operator
  print ("(negate 3 +)", (negate 3 +) 4)
  print ("(+ negate 3)", (+ negate 3) 4)
  print ("(`div` succ 1)", (`div` succ 1) 9)
  -- low-precedence operators
  print ("($ 1 + 2)", ($ 1 + 2) negate)
  print ("(f . g $)", (negate . abs $) 4)
