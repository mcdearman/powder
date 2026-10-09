-- Haskell 2010 Report 11.4: derived Read for infix constructors honours the precedence of the constructor but NOT its
-- associativity: both operands are read at precedence p+1, so "a :+: b :+: c" has no complete parse even for infixr
module Main (main) where

infixr 5 :+:

infixl 6 :-:

infix 4 :=:

-- (:*:) has no fixity declaration: infixl 9

data E = L Int | E :+: E | E :-: E | E :=: E | E :*: E deriving (Eq, Show, Read)

infixr 5 :+

infixl 6 :-

-- operands read at precedence 6
data A = Int :+ Int deriving (Eq, Show, Read)

-- operands read at precedence 7
data B = Int :- Int deriving (Eq, Show, Read)

-- every complete parse of the string
full :: String -> [E]
full s = [x | (x, "") <- reads s]

main :: IO ()
main = do
  print ("simple", read "L 1 :+: L 2" :: E, read "L 1 :-: L 2" :: E, read "L 1 :=: L 2" :: E, read "L 1 :*: L 2" :: E)
  print ("white space", read "  L 1   :+:   L 2  " :: E, read "L 1:+:L 2" :: E, read "L 1\n:+:\nL 2" :: E)
  print ("parentheses", read "(L 1 :+: L 2)" :: E, read "((L 1) :+: (L 2))" :: E, read " ( L (1) :+: L (2) ) " :: E)
  print ("right nested, parenthesised", read "L 1 :+: (L 2 :+: L 3)" :: E)
  print ("left nested, parenthesised", read "(L 1 :+: L 2) :+: L 3" :: E)
  print ("infixr without parentheses: no complete parse", full "L 1 :+: L 2 :+: L 3")
  print ("infixl without parentheses: no complete parse", full "L 1 :-: L 2 :-: L 3", full "L 1 :*: L 2 :*: L 3")
  print ("infix without parentheses: no complete parse", full "L 1 :=: L 2 :=: L 3")
  print ("infixl parenthesised", read "(L 1 :-: L 2) :-: L 3" :: E, read "L 1 :-: (L 2 :-: L 3)" :: E)
  print ("tighter operator on the left", read "L 1 :-: L 2 :+: L 3" :: E, (read "L 1 :-: L 2 :+: L 3" :: E) == (L 1 :-: L 2) :+: L 3)
  print ("tighter operator on the right", read "L 1 :+: L 2 :-: L 3" :: E, (read "L 1 :+: L 2 :-: L 3" :: E) == L 1 :+: (L 2 :-: L 3))
  print ("tightest", read "L 1 :*: L 2 :+: L 3 :*: L 4" :: E)
  print ("four levels", (read "L 1 :*: L 2 :-: L 3 :*: L 4 :+: L 5 :=: L 6" :: E) == (L 1 :*: L 2 :-: L 3 :*: L 4 :+: L 5 :=: L 6))
  print ("looser operator needs parentheses", read "(L 1 :+: L 2) :-: L 3" :: E, read "(L 1 :=: L 2) :+: L 3" :: E, read "L 1 :*: (L 2 :+: L 3)" :: E)
  print ("looser operator parses the other way", (read "L 1 :+: L 2 :*: L 3" :: E) == L 1 :+: (L 2 :*: L 3))
  print ("negative arguments", read "L (-1) :+: L (-2)" :: E, read "L (-1) :*: L (-2)" :: E)
  print ("in structures", read "Just (L 1 :+: L 2)" :: Maybe E, read "[L 1 :+: L 2,L 3 :*: L 4]" :: [E], read "(L 1 :=: L 2,L 3 :-: L 4)" :: (E, E))
  print ("unparenthesised infix as an argument fails", null (reads "Just L 1 :+: L 2" :: [(Maybe E, String)]) || null [() | (_, "") <- reads "Just L 1 :+: L 2" :: [(Maybe E, String)]])
  print ("prefix form of an infix constructor is not read", full "(:+:) (L 1) (L 2)", null (reads "(:+) 1 2" :: [(A, String)]))
  print ("numbers as operands", read "1 :+ 2" :: A, read "1:+2" :: A, read "(1) :+ (2)" :: A, read "1 :- 2" :: B)
  print ("negative operands", read "(-1) :+ (-2)" :: A, read "-1 :+ -2" :: A, read "(-1) :- (-2)" :: B)
  print ("number operand failures", null (reads ":+ 2" :: [(A, String)]), null (reads "1 :- " :: [(B, String)]), null (reads "1 + 2" :: [(A, String)]))
  print ("read . show", all (\e -> read (show e) == e) [L 1 :+: (L 2 :+: L 3), (L 1 :+: L 2) :+: L 3, L 1 :-: L 2 :+: L 3, (L 1 :=: L 2) :*: L (-3), L 1 :*: L 2 :*: L 3])
  print ("read . show numbers", all (\a -> read (show a) == a) [1 :+ 2, (-1) :+ (-2)], all (\b -> read (show b) == b) [1 :- 2, (-1) :- (-2)])
