-- Haskell 2010 Report 4.2.1: parameterised and recursive algebraic datatypes
module Main (main) where

data List a = Nil | Cons a (List a)

data Tree a = Leaf | Node (Tree a) a (Tree a)

data Pair a b = Pair a b

data Opt a = None | Some a

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

fromList :: [a] -> List a
fromList = foldr Cons Nil

insertT :: Ord a => a -> Tree a -> Tree a
insertT x Leaf = Node Leaf x Leaf
insertT x t@(Node l v r)
  | x < v = Node (insertT x l) v r
  | x > v = Node l v (insertT x r)
  | otherwise = t

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node l v r) = inorder l ++ [v] ++ inorder r

depth :: Tree a -> Int
depth Leaf = 0
depth (Node l _ r) = 1 + max (depth l) (depth r)

swapPair :: Pair a b -> Pair b a
swapPair (Pair x y) = Pair y x

unPair :: Pair a b -> (a, b)
unPair (Pair x y) = (x, y)

optToList :: Opt a -> [a]
optToList None = []
optToList (Some x) = [x]

eval :: Expr -> Int
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Neg e) = negate (eval e)

main :: IO ()
main = do
  print ("list", toList (Cons 1 (Cons 2 (Cons (3 :: Int) Nil))))
  print ("list roundtrip", toList (fromList "abc"))
  print ("list empty", toList (Nil :: List Bool))
  print ("list of lists", map toList (toList (fromList [fromList [True], Nil])))
  print ("tree inorder", inorder (foldr insertT Leaf [5, 2, 8, 1, 9, 2 :: Int]))
  print ("tree depth", depth (foldr insertT Leaf [1, 2, 3, 4 :: Int]))
  print ("tree chars", inorder (foldr insertT Leaf "haskell"))
  print ("tree empty depth", depth (Leaf :: Tree Int))
  print ("pair", unPair (swapPair (Pair 'a' True)))
  print ("pair nested", unPair (Pair (unPair (Pair (1 :: Int) 'x')) "s"))
  print ("opt", optToList (Some 'x'), optToList (None :: Opt Char))
  print ("expr", eval (Add (Lit 2) (Mul (Lit 3) (Neg (Lit 4)))))
