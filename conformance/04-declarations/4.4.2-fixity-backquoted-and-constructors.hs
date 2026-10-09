-- Haskell 2010 Report 4.4.2: fixity of backquoted functions and of constructors; several operators in one declaration
module Main (main) where

-- several operators (symbolic and backquoted) in one declaration, before their definitions
infixl 6 `plus`, `minus`, <+>
infix 4 `sameAs`
infixr 5 :+:
infixr 2 `Or`
infixr 3 `And`

plus, minus, (<+>) :: Int -> Int -> Int
plus a b = a + b
minus a b = a - b
a <+> b = a + b

sameAs :: Int -> Int -> Bool
sameAs a b = a == b

data Chain = End | Int :+: Chain

data B = T | F | B `Or` B | B `And` B

render :: B -> String
render T = "T"
render F = "F"
render (a `Or` b) = "(" ++ render a ++ "|" ++ render b ++ ")"
render (a `And` b) = "(" ++ render a ++ "&" ++ render b ++ ")"

-- fixity applies in patterns too: a :+: (b :+: rest)
firstTwo :: Chain -> (Int, Int)
firstTwo (a :+: b :+: _) = (a, b)
firstTwo (a :+: End) = (a, 0)
firstTwo End = (0, 0)

sumChain :: Chain -> Int
sumChain End = 0
sumChain (x :+: rest) = x + sumChain rest

-- declared after its uses above and below
times :: Int -> Int -> Int
times a b = a * b

infixl 7 `times`

main :: IO ()
main = do
  print ("infixl backquoted", 10 `minus` 3 `plus` 2 `minus` 1)
  print ("backquoted with operator", 10 `minus` 3 <+> 2 `minus` 1)
  print ("infix 4 looser", 1 `plus` 2 `sameAs` 3, 2 <+> 2 `sameAs` 5)
  print ("against Prelude *", 2 * 3 `plus` 4, 2 `plus` 3 * 4)
  print ("times tighter than plus", 1 `plus` 2 `times` 3, 2 `times` 3 `plus` 1)
  print ("times same as *", 2 * 3 `times` 4, 12 `div` 2 `times` 3)
  print ("backquoted sections", (`minus` 1) 5, (10 `minus`) 4)
  print ("constructor infixr", sumChain (1 :+: 2 :+: 3 :+: End))
  print ("constructor in pattern", firstTwo (1 :+: 2 :+: 3 :+: End), firstTwo (7 :+: End))
  print ("constructor against +", sumChain (1 + 1 :+: 2 * 2 :+: End))
  print ("backquoted constructors", render (T `Or` F `And` T `Or` F))
  print ("and tighter than or", render (T `And` F `Or` T `And` F))
  print ("and right assoc", render (T `And` F `And` T))
