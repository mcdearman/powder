-- Haskell 2010 Report 4.4.1: type signatures in let and where declaration groups
module Main (main) where

sumSquares :: [Int] -> Int
sumSquares xs = total (map square xs)
  where
    square :: Int -> Int
    square x = x * x
    total :: [Int] -> Int
    total = foldr (+) 0

-- a local signature with its own (fresh) type variables makes the local polymorphic
pairs :: (String, [Bool])
pairs = (twice "ab", twice [True])
  where
    twice :: [a] -> [a]
    twice ys = ys ++ ys

-- a local signature lets a pattern-bound overloaded value be used at two types
bothTypes :: (Int, Double)
bothTypes =
  let plus :: Num a => a -> a -> a
      plus = (+)
  in (plus 1 2, plus 1.5 2)

-- one local signature for several variables, and one for an operator
localOps :: Int
localOps = (a <+> b) <+> c
  where
    a, b, c :: Int
    a = 1
    b = 20
    c = 300
    (<+>) :: Int -> Int -> Int
    x <+> y = x + y

-- a local signature more specific than the inferred type
restricted :: [Int]
restricted =
  let ident :: Int -> Int
      ident z = z
  in map ident [1, 2, 3]

-- signature for variables bound by a local pattern binding
fromPattern :: Int
fromPattern = q * 10 + r
  where
    q, r :: Int
    (q, r) = 47 `divMod` 5

main :: IO ()
main = do
  print ("where signatures", sumSquares [1, 2, 3])
  print ("local polymorphic", pairs)
  print ("local overloaded", bothTypes)
  print ("several and operator", localOps)
  print ("local restricted", restricted)
  print ("local pattern binding", fromPattern)
  let showTwice :: Show a => a -> String
      showTwice v = show v ++ show v
  print ("do-let signature", showTwice 'c', showTwice (12 :: Int))
