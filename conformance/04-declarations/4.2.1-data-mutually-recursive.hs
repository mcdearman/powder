-- Haskell 2010 Report 4.2.1: mutually recursive datatype declarations
module Main (main) where

data EvenN = Zero | SuccOdd OddN

data OddN = SuccEven EvenN

data Rose a = Rose a (Forest a)

data Forest a = NoTrees | Trees (Rose a) (Forest a)

data Stmt = Assign String Exp | Block [Stmt]

data Exp = Lit Int | Var String | After Stmt Exp

evenToInt :: EvenN -> Int
evenToInt Zero = 0
evenToInt (SuccOdd o) = 1 + oddToInt o

oddToInt :: OddN -> Int
oddToInt (SuccEven e) = 1 + evenToInt e

sizeRose :: Rose a -> Int
sizeRose (Rose _ f) = 1 + sizeForest f

sizeForest :: Forest a -> Int
sizeForest NoTrees = 0
sizeForest (Trees r f) = sizeRose r + sizeForest f

flatten :: Rose a -> [a]
flatten (Rose x f) = x : flattenForest f

flattenForest :: Forest a -> [a]
flattenForest NoTrees = []
flattenForest (Trees r f) = flatten r ++ flattenForest f

assigned :: Stmt -> [String]
assigned (Assign v e) = assignedExp e ++ [v]
assigned (Block ss) = concatMap assigned ss

assignedExp :: Exp -> [String]
assignedExp (Lit _) = []
assignedExp (Var _) = []
assignedExp (After s e) = assigned s ++ assignedExp e

sample :: Rose Char
sample = Rose 'a' (Trees (Rose 'b' (Trees (Rose 'c' NoTrees) NoTrees)) (Trees (Rose 'd' NoTrees) NoTrees))

main :: IO ()
main = do
  print ("even zero", evenToInt Zero)
  print ("even four", evenToInt (SuccOdd (SuccEven (SuccOdd (SuccEven Zero)))))
  print ("odd three", oddToInt (SuccEven (SuccOdd (SuccEven Zero))))
  print ("rose size", sizeRose sample)
  print ("rose flatten", flatten sample)
  print ("forest empty", sizeForest (NoTrees :: Forest Int))
  print ("stmt", assigned (Block [Assign "x" (Lit 1), Assign "y" (After (Assign "z" (Var "x")) (Lit 2))]))
  print ("stmt empty", assigned (Block []))
