{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
-- PatternSynonyms: a name for a pattern, which with = is an expression too; here and from another module
module Main (main) where

import Shapes (Shape, pattern Square, pattern Circle, pattern Origin, pattern Sized, area)
import qualified Shapes as S

data Expr = Lit Int | BinOp Char Expr Expr deriving (Show)

pattern Add :: Expr -> Expr -> Expr
pattern Add a b = BinOp '+' a b

pattern Mul a b = BinOp '*' a b

pattern Zero :: Expr
pattern Zero = Lit 0

pattern Double' e <- Add e ((sameAs e) -> True)

-- Written between its variables, and by way of another synonym.
pattern a :+: b = Add a b

pattern Three a b c = (a, (b, c))

pattern Head x <- x : _

pattern Even <- (even -> True)

pattern Pair a b = (a, b)

sameAs :: Expr -> Expr -> Bool
sameAs (Lit a) (Lit b) = a == b
sameAs _ _ = False

simplify :: Expr -> Expr
simplify (Add Zero e) = simplify e
simplify (Add e Zero) = simplify e
simplify (Mul Zero _) = Zero
simplify (Double' e) = Mul (Lit 2) e
simplify (Add a b) = Add (simplify a) (simplify b)
simplify (Mul a b) = Mul (simplify a) (simplify b)
simplify e = e

eval :: Expr -> Int
eval (Lit n) = n
eval (a :+: b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (BinOp _ _ _) = 0

classify :: Int -> String
classify Even = "even"
classify _ = "odd"

main :: IO ()
main = do
  print (simplify (Add Zero (Lit 5)))
  print (simplify (Add (Lit 3) (Lit 3)))
  print (simplify (Mul Zero (Lit 9)), Add (Lit 1) Zero)
  print (eval (Mul (Lit 2) (Lit 3 :+: Lit 4)))
  print (case Three 'a' True "s" of Three a b c -> (c, b, a))
  print (case "xyz" of Head c -> c)
  print (map classify [1, 2, 3])
  print (case Pair 1 'p' of Pair n c -> (c, n + 1 :: Int))
  print [n | Pair n Even <- [(1 :: Int, 2 :: Int), (3, 3), (5, 6)]]
  print ((\(Pair a b) -> a + b) (Pair 3 (4 :: Int)))
  -- From another module, where the types they are of are abstract.
  print (map describe [Square 2, Circle 3, Origin])
  print (case Circle 2 of Sized n -> n, case Square 2 of S.Sized n -> n)
  print (map area [Square 2, Circle 1])
  where
    describe :: Shape -> String
    describe Origin = "origin"
    describe (Square s) = "square " ++ show s
    describe (S.Circle r) = "circle " ++ show r
    describe (Sized n) = "sized " ++ show n
