{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GADTs, KindSignatures, RoleAnnotations, RankNTypes #-}
-- GADTs: a constructor may make its type at types of its own choosing, and matching it says what those are
module Main (main) where

import Data.Kind (Type)

-- An expression that says what type it is of, so that evaluating needs no tags.
data Expr a where
  IntE :: Int -> Expr Int
  BoolE :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  Eq :: Expr Int -> Expr Int -> Expr Bool
  Pair :: Expr a -> Expr b -> Expr (a, b)
  Fst :: Expr (a, b) -> Expr a
  Not :: Expr Bool -> Expr Bool

eval :: Expr a -> a
eval (IntE n) = n
eval (BoolE b) = b
eval (Add x y) = eval x + eval y
eval (If c t e) = if eval c then eval t else eval e
eval (Eq x y) = eval x == eval y
eval (Pair x y) = (eval x, eval y)
eval (Fst p) = fst (eval p)
eval (Not b) = not (eval b)

pretty :: Expr a -> String
pretty e = case e of
  IntE n -> show n
  BoolE b -> show b
  Add x y -> "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
  If c t f -> "if " ++ pretty c ++ " then " ++ pretty t ++ " else " ++ pretty f
  Eq x y -> pretty x ++ " == " ++ pretty y
  Pair x y -> "(" ++ pretty x ++ ", " ++ pretty y ++ ")"
  Fst p -> "fst " ++ pretty p
  Not b -> "not " ++ pretty b

size :: Expr a -> Int
size (Add x y) = 1 + size x + size y
size (If c t e) = 1 + size c + size t + size e
size (Eq x y) = 1 + size x + size y
size (Pair x y) = 1 + size x + size y
size (Fst p) = 1 + size p
size (Not b) = 1 + size b
size _ = 1

-- A type that is its own evidence: what type a value is of.
data Rep :: Type -> Type where
  RInt :: Rep Int
  RBool :: Rep Bool
  RList :: Rep a -> Rep [a]
  RPair :: Rep a -> Rep b -> Rep (a, b)

type role Rep nominal

showWith :: Rep a -> a -> String
showWith RInt n = show n
showWith RBool b = if b then "yes" else "no"
showWith (RList r) xs = "[" ++ concatMap ((++ ";") . showWith r) xs ++ "]"
showWith (RPair r s) (a, b) = "<" ++ showWith r a ++ "|" ++ showWith s b ++ ">"

zero :: Rep a -> a
zero RInt = 0
zero RBool = False
zero (RList _) = []
zero (RPair r s) = (zero r, zero s)

-- Evidence that two types are the same, which matching makes use of.
data Equal a b where
  Refl :: Equal a a

castWith :: Equal a b -> a -> b
castWith Refl x = x

sameRep :: Rep a -> Rep b -> Maybe (Equal a b)
sameRep RInt RInt = Just Refl
sameRep RBool RBool = Just Refl
sameRep (RList r) (RList s) = case sameRep r s of
  Just Refl -> Just Refl
  Nothing -> Nothing
sameRep (RPair r s) (RPair r' s') = case sameRep r r' of
  Just Refl -> case sameRep s s' of
    Just Refl -> Just Refl
    Nothing -> Nothing
  Nothing -> Nothing
sameRep _ _ = Nothing

-- With a context, and a type variable of its own, beside the type it chooses.
data Some where
  Some :: Show a => Rep a -> a -> Some

describe :: Some -> String
describe (Some r x) = show x ++ " as " ++ showWith r x

-- Lists that say in their type whether they are empty.
data Empty

data NonEmpty

data Safe a n where
  Nil :: Safe a Empty
  Cons :: a -> Safe a n -> Safe a NonEmpty

safeHead :: Safe a NonEmpty -> a
safeHead (Cons x _) = x

toList :: Safe a n -> [a]
toList Nil = []
toList (Cons x rest) = x : toList rest

-- That two types are the same, asked for in a context.
same :: (a ~ b) => a -> b -> [a]
same x y = [x, y]

class Collection c where
  type' :: c -> String

instance (a ~ Char) => Collection [a] where
  type' s = "a string of " ++ show (length s)

main :: IO ()
main = do
  let e = If (Eq (Add (IntE 1) (IntE 2)) (IntE 3)) (Pair (IntE 10) (BoolE True)) (Pair (IntE 0) (Not (BoolE True)))
  print (eval e, eval (Fst e), eval (Add (IntE 20) (Fst e)))
  putStrLn (pretty e)
  print (size e, eval (Not (Eq (IntE 1) (IntE 1))))
  putStrLn (showWith (RList (RPair RInt RBool)) [(1, True), (2, False)])
  print (zero (RPair RInt (RList RBool)), zero RInt + 1)
  print (castWith Refl 'c', fmap (`castWith` (5 :: Int)) (sameRep RInt RInt))
  print (case sameRep (RList RInt) (RList RInt) of Just Refl -> "same"; Nothing -> "different")
  print (case sameRep (RList RInt) (RList RBool) of Just Refl -> "same"; Nothing -> "different")
  putStrLn (describe (Some RInt 5) ++ ", " ++ describe (Some (RList RBool) [True]))
  print (safeHead (Cons 'a' (Cons 'b' Nil)), toList (Cons 1 (Cons 2 (Cons (3 :: Int) Nil))), toList (Nil :: Safe Int Empty))
  print (same 'a' 'b')
  putStrLn (type' "four")
