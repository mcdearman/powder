-- Haskell 2010 Report 11.4: derived Show for infix constructors whose arguments are numbers, prefix applications or
-- other infix constructors: each argument is shown at precedence p+1, where p is the constructor's precedence
module Main (main) where

infixr 5 :+

infixl 6 :-

infix 4 :==

infixl 7 :*

infixr 0 :$

infixl 9 :!

-- arguments shown at precedence 6: a negative number is not parenthesised
data A = Int :+ Int deriving (Show)

-- arguments shown at precedence 7: a negative number is parenthesised
data B = Int :- Int deriving (Show)

-- arguments shown at precedence 5
data C = Double :== Double deriving (Show)

-- arguments shown at precedence 8
data D = Int :* Int deriving (Show)

-- arguments shown at precedence 1
data F = Int :$ Maybe Int deriving (Show)

-- arguments shown at precedence 10: applications are not parenthesised, negative numbers are
data G = Maybe Int :! Maybe Int deriving (Show)

-- no fixity declaration (infixl 9): arguments shown at precedence 10
data H = Int :? Int deriving (Show)

-- a type mixing prefix and infix constructors
data M = Leaf Int | M :^: M | Neg M | Pair M M deriving (Show)

infixr 8 :^:

main :: IO ()
main = do
  print (1 :+ 2, (-1) :+ 2, 1 :+ (-2), (-1) :+ (-2))
  print (1 :- 2, (-1) :- 2, 1 :- (-2), (-1) :- (-2))
  print (1.5 :== 2.5, (-1.5) :== 2.5, 1.5 :== (-2.5))
  print (1 :* 2, (-1) :* 2, 1 :* (-2))
  print (1 :$ Just 2, (-1) :$ Just (-2), (-1) :$ Nothing)
  print (Just 1 :! Just 2, Just (-1) :! Nothing, Nothing :! Just (-2))
  print (1 :? 2, (-1) :? 2, 1 :? (-2))
  putStrLn (showsPrec 5 ((-1) :+ (-2)) "" ++ " | " ++ showsPrec 6 ((-1) :+ (-2)) "" ++ " | " ++ showsPrec 11 (1 :+ 2) "")
  putStrLn (showsPrec 6 ((-1) :- 2) "" ++ " | " ++ showsPrec 7 ((-1) :- 2) "")
  putStrLn (showsPrec 4 (1 :== 2) "" ++ " | " ++ showsPrec 5 (1 :== 2) "")
  putStrLn (showsPrec 0 (1 :$ Just 2) "" ++ " | " ++ showsPrec 1 (1 :$ Just 2) "" ++ " | " ++ showsPrec 11 (1 :$ Nothing) "")
  putStrLn (showsPrec 9 (Just 1 :! Nothing) "" ++ " | " ++ showsPrec 10 (Just 1 :! Nothing) "" ++ " | " ++ showsPrec 11 (Just 1 :! Nothing) "")
  print (Just (1 :+ 2), Just ((-1) :- 2), [1 :* 2], Left (1 :? 2) :: Either H Int)
  print (Leaf 1 :^: Leaf 2)
  print (Leaf (-1) :^: Leaf (-2))
  print (Neg (Leaf 1) :^: Neg (Leaf (-2)))
  print (Neg (Leaf 1 :^: Leaf 2))
  print (Pair (Leaf 1 :^: Leaf 2) (Neg (Leaf 3)))
  print (Pair (Leaf 1) (Leaf 2) :^: Pair (Leaf 3) (Leaf 4))
  print (Leaf 1 :^: Leaf 2 :^: Leaf 3)
  print ((Leaf 1 :^: Leaf 2) :^: Leaf 3)
  print (Neg (Neg (Leaf 1 :^: Neg (Leaf 2 :^: Leaf 3))))
  print (Just (Leaf 1 :^: Leaf 2), [Neg (Leaf 1) :^: Leaf 2], (Leaf 1 :^: Leaf 2, Neg (Leaf 0)))
  putStrLn (showsPrec 8 (Leaf 1 :^: Leaf 2) "" ++ " | " ++ showsPrec 9 (Leaf 1 :^: Leaf 2) "" ++ " | " ++ showsPrec 11 (Neg (Leaf 1)) "")
