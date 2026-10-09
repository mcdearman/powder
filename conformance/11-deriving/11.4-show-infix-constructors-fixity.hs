-- Haskell 2010 Report 11.4: derived showsPrec for an infix constructor of precedence p is
--   showParen (d > p) (showsPrec (p+1) left . showString " op " . showsPrec (p+1) right)
-- Associativity is NOT used: `a :+: (b :+: c)` keeps its parentheses even though :+: is infixr.
module Main (main) where

infixr 5 :+:

infixl 6 :-:

infix 4 :=:

-- (:*:) has no fixity declaration: infixl 9

data E = L Int | E :+: E | E :-: E | E :=: E | E :*: E deriving (Show)

table :: String -> E -> IO ()
table label x = mapM_ (\d -> putStrLn (label ++ " @" ++ show d ++ ": " ++ showsPrec d x "")) [0 .. 11 :: Int]

main :: IO ()
main = do
  table "infixr 5" (L 1 :+: L 2)
  table "infixl 6" (L 1 :-: L 2)
  table "infix 4" (L 1 :=: L 2)
  table "default infixl 9" (L 1 :*: L 2)
  -- associativity is ignored: both nestings of the same operator are parenthesised
  print (L 1 :+: (L 2 :+: L 3))
  print ((L 1 :+: L 2) :+: L 3)
  print (L 1 :+: L 2 :+: L 3)
  print (L 1 :-: (L 2 :-: L 3))
  print ((L 1 :-: L 2) :-: L 3)
  print (L 1 :-: L 2 :-: L 3)
  print (L 1 :*: (L 2 :*: L 3))
  print (L 1 :*: L 2 :*: L 3)
  print ((L 1 :=: L 2) :=: L 3)
  print (L 1 :=: (L 2 :=: L 3))
  -- precedence is used: a tighter operator inside a looser one needs no parentheses
  print (L 1 :-: L 2 :+: L 3)
  print (L 1 :+: L 2 :-: L 3)
  print ((L 1 :+: L 2) :-: L 3)
  print (L 1 :+: L 2 :=: L 3 :-: L 4)
  print ((L 1 :=: L 2) :+: L 3)
  print (L 1 :*: L 2 :+: L 3 :*: L 4)
  print ((L 1 :+: L 2) :*: (L 3 :-: L 4))
  print (L 1 :*: L 2 :-: L 3 :*: L 4 :+: L 5 :=: L 6)
  -- negative literals inside prefix constructors inside infix constructors
  print (L (-1) :+: L (-2))
  print (L (-1) :*: L (-2))
  -- inside other structures
  print (Just (L 1 :+: L 2))
  print [L 1 :+: L 2, L 3 :*: L 4]
  print (L 1 :=: L 2, L 3 :-: L 4)
  print (Left (L 1 :*: L 2) :: Either E Int)
  table "nested" ((L 1 :+: L 2) :-: (L 3 :=: L 4))
