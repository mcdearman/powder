{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
-- DataKinds: a data type is a kind, and its constructors are the types of it
module Main (main) where

import Data.Kind (Type)

data Nat = Z | S Nat

-- A list that says in its type how long it is.
data Vec (n :: Nat) a where
  Nil :: Vec 'Z a
  Cons :: a -> Vec n a -> Vec ('S n) a

vhead :: Vec ('S n) a -> a
vhead (Cons x _) = x

vtail :: Vec ('S n) a -> Vec n a
vtail (Cons _ xs) = xs

toList :: Vec n a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap _ Nil = Nil
vmap f (Cons x xs) = Cons (f x) (vmap f xs)

vzip :: Vec n a -> Vec n b -> Vec n (a, b)
vzip Nil Nil = Nil
vzip (Cons x xs) (Cons y ys) = Cons (x, y) (vzip xs ys)

-- A function from types of the kind to one of them.
type family Plus (a :: Nat) (b :: Nat) :: Nat where
  Plus 'Z b = b
  Plus ('S a) b = 'S (Plus a b)

append :: Vec n a -> Vec m a -> Vec (Plus n m) a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

-- A constructor without its tick, where no type goes by the name.
data Door (s :: State) where
  Opened :: Door Open
  Closed :: Door Shut

data State = Open | Shut

shut :: Door Open -> Door Shut
shut Opened = Closed

open :: Door Shut -> Door Open
open Closed = Opened

describe :: Door s -> String
describe Opened = "open"
describe Closed = "shut"

-- A list of types, as a type.
data HList (ts :: [Type]) where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

hhead :: HList (t ': ts) -> t
hhead (HCons x _) = x

htail :: HList (t ': ts) -> HList ts
htail (HCons _ xs) = xs

hlength :: HList ts -> Int
hlength HNil = 0
hlength (HCons _ xs) = 1 + hlength xs

triple :: HList '[Int, Bool, String]
triple = HCons 1 (HCons True (HCons "three" HNil))

type family Length (ts :: [Type]) :: Nat where
  Length '[] = 'Z
  Length (t ': ts) = 'S (Length ts)

-- A type that stands for one of the kind, and the class that says which.
data Tagged (n :: Nat) = Tagged

class Known (n :: Nat) where
  known :: Tagged n -> Int

instance Known 'Z where
  known _ = 0

instance Known n => Known ('S n) where
  known t = 1 + known (pred' t)

pred' :: Tagged ('S n) -> Tagged n
pred' Tagged = Tagged

lengthOf :: HList ts -> Tagged (Length ts)
lengthOf _ = Tagged

main :: IO ()
main = do
  let v = Cons 1 (Cons 2 (Cons (3 :: Int) Nil))
  print (vhead v, toList (vtail v), toList (vmap (* 2) v))
  print (toList (vzip v (vmap show v)))
  print (toList (append v (Cons 4 Nil)), vhead (vtail (append (Cons 'a' Nil) (Cons 'b' Nil))))
  putStrLn (describe (shut Opened) ++ ", " ++ describe (open (shut Opened)))
  print (hhead triple, hhead (htail triple), hhead (htail (htail triple)), hlength triple)
  print (known (Tagged :: Tagged ('S ('S ('S 'Z)))), known (lengthOf triple))
