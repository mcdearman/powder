-- Haskell 2010 Report 11: derived instances for recursive and mutually recursive data types
module Main (main) where

data Nat = Z | S Nat deriving (Eq, Ord, Show, Read)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show, Read)

data Rose a = Rose a [Rose a] deriving (Eq, Ord, Show, Read)

-- mutually recursive pair of types
data EvenN = EZero | ESucc OddN deriving (Eq, Ord, Show, Read)

data OddN = OSucc EvenN deriving (Eq, Ord, Show, Read)

-- mutually recursive and parameterised: the contexts of the two instances depend on each other
data Forest a = Forest [Bush a] deriving (Eq, Ord, Show, Read)

data Bush a = Bush a (Forest a) deriving (Eq, Ord, Show, Read)

-- a type that refers to a later-declared type
data Stmt = Assign String Expr | Block [Stmt] deriving (Eq, Ord, Show, Read)

data Expr = Lit Int | Var String | Do Stmt Expr deriving (Eq, Ord, Show, Read)

nat :: Int -> Nat
nat 0 = Z
nat n = S (nat (n - 1))

tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

rose :: Rose Char
rose = Rose 'a' [Rose 'b' [], Rose 'c' [Rose 'd' []]]

four :: EvenN
four = ESucc (OSucc (ESucc (OSucc EZero)))

forest :: Forest Int
forest = Forest [Bush 1 (Forest []), Bush 2 (Forest [Bush 3 (Forest [])])]

prog :: Stmt
prog = Block [Assign "x" (Lit 1), Assign "y" (Do (Assign "z" (Var "x")) (Lit (-2)))]

main :: IO ()
main = do
  print (nat 3)
  print ("Nat", nat 3 == nat 3, nat 3 == nat 4, nat 3 < nat 4, compare (nat 5) (nat 2), maximum (map nat [1, 3, 2]) == nat 3)
  print ("Nat read", read "S (S Z)" == nat 2, read (show (nat 6)) == nat 6)
  print tree
  print ("Tree", tree == tree, tree == Leaf, Leaf < tree, compare tree (Node Leaf 0 Leaf), read (show tree) == tree)
  print rose
  print ("Rose", rose == rose, rose < Rose 'a' [Rose 'c' []], read (show rose) == rose)
  print four
  print ("EvenN/OddN", four == four, four > EZero, OSucc four > OSucc EZero, read (show four) == four, read "OSucc EZero" == OSucc EZero)
  print forest
  print ("Forest/Bush", forest == forest, Forest [] < forest, Bush 'a' (Forest []) < Bush 'b' (Forest []), read (show forest) == forest)
  print prog
  print ("Stmt/Expr", prog == prog, prog > Assign "zzz" (Lit 9), Lit 5 < Var "a", read (show prog) == prog)
  print ("deep", nat 2000 == nat 2000, nat 2000 < nat 2001, length (show (nat 100)))
