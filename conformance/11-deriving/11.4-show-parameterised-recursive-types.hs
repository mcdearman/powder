-- Haskell 2010 Report 11.4: derived Show for parameterised and recursive types (lists, trees, Maybe/Either-like)
module Main (main) where

data List a = Nil | Cons a (List a) deriving (Show)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

-- the Report's own example type from section 11.5 (infix constructor of precedence 5)
infixr 5 :^:

data RTree a = RLeaf a | RTree a :^: RTree a deriving (Show)

data Opt a = None | Some a deriving (Show)

data Alt a b = L a | R b deriving (Show)

data Rose a = Rose a [Rose a] deriving (Show)

data Nat = Z | S Nat deriving (Show)

fromL :: [a] -> List a
fromL = foldr Cons Nil

leaf :: a -> Tree a
leaf x = Node Leaf x Leaf

main :: IO ()
main = do
  print (Nil :: List Int)
  print (Cons 1 Nil :: List Int)
  print (Cons 1 (Cons 2 Nil) :: List Int)
  print (fromL [-1, 2, -3 :: Int])
  print (fromL "ab")
  print (fromL ["ab", ""])
  print (fromL [Just 1, Nothing :: Maybe Int])
  print (fromL [fromL [1 :: Int], Nil])
  print (Leaf :: Tree Int)
  print (leaf 'x')
  print (Node (leaf 1) 2 (leaf (3 :: Int)))
  print (Node (Node (leaf (-1)) 0 Leaf) 1 (leaf (2 :: Int)))
  print (leaf (leaf True))
  print (RLeaf 1 :: RTree Int)
  print (RLeaf 1 :^: RLeaf (2 :: Int))
  print (RLeaf 1 :^: RLeaf 2 :^: RLeaf (3 :: Int))
  print ((RLeaf 1 :^: RLeaf 2) :^: RLeaf (3 :: Int))
  print (RLeaf (-1) :^: RLeaf (-2 :: Int))
  print (RLeaf "s" :^: (RLeaf "t" :^: RLeaf "u") :^: RLeaf "v")
  putStrLn (showsPrec 5 (RLeaf 'a' :^: RLeaf 'b') "" ++ " | " ++ showsPrec 6 (RLeaf 'a' :^: RLeaf 'b') "" ++ " | " ++ showsPrec 11 (RLeaf 'a') "")
  print (None :: Opt Int, Some 1 :: Opt Int, Some (-1) :: Opt Int)
  print (Some (Some 'a'), Some (None :: Opt Char), Some (Some (Some ())))
  print (Some [1, 2 :: Int], Some "str", Some (1 :: Int, 'c'), [Some True, None])
  print (L 1 :: Alt Int Char, R 'c' :: Alt Int Char, L (R (-1)) :: Alt (Alt Char Int) Bool)
  print (Rose 1 [] :: Rose Int)
  print (Rose 'a' [Rose 'b' [], Rose 'c' [Rose 'd' []]])
  print (Rose (-1) [Rose (-2) []] :: Rose Int)
  print (S (S (S Z)), [Z, S Z], Just (S Z))
  print (Some (leaf (Cons (L 1) Nil)) :: Opt (Tree (List (Alt Int Bool))))
  print (Just (Cons 1 Nil :: List Int), Left Leaf :: Either (Tree Int) Int)
  print (length (show (fromL [1 .. 1000 :: Int])))
