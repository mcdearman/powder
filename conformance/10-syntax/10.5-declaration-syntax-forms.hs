-- Haskell 2010 Report 10.5: context-free syntax corner cases of declarations: funlhs forms, guards, where, multiple names
module Main (main) where

infixl 6 |+|
infix 4 `sameAs`

-- funlhs: var apat {apat}
plain :: Int -> Int -> Int
plain a b = a + b

-- funlhs: pat varop pat
(|+|) :: Int -> Int -> Int
a |+| b = a + b

sameAs :: Int -> Int -> Bool
a `sameAs` b = a == b

-- funlhs: ( funlhs ) apat {apat}
(|-|) :: Int -> Int -> Int -> Int
(a |-| b) c = a - b - c

applyTwice :: (Int -> Int) -> Int -> Int
f `applyTwice` x = f (f x)

-- infix definition with constructor patterns on both sides
merge :: [Int] -> [Int] -> [Int]
[] `merge` ys = ys
xs `merge` [] = xs
(x : xs) `merge` (y : ys)
  | x <= y = x : (xs `merge` (y : ys))
  | otherwise = y : ((x : xs) `merge` ys)

-- operator defined in prefix form
(<+>) :: Int -> Int -> Int
(<+>) a b = a * b

-- several names in one signature, several guards, where over all guards
low, mid, high :: Int
low = 1
mid = 5
high = 9

band :: Int -> String
band n
  | n < lo = "low"
  | n < hi = "mid"
  | otherwise = "high"
  where
    lo = mid
    hi = high

-- pattern bindings of several shapes
(p1, p2) = (10 :: Int, "ten")
[q1, q2] = [1 :: Int, 2]
Just j1 = Just 'j'
(h1 : _) = "head"

-- unary minus pattern, and negative literal on an infix lhs
isMinusOne :: Int -> Bool
isMinusOne (-1) = True
isMinusOne _ = False

-- a guard in a pattern binding
gv :: Int
gv | low > 5 = 0
   | otherwise = 42

main :: IO ()
main = do
  print ("plain", plain 1 2, 1 `plain` 2)
  print ("varop lhs", 1 |+| 2, 3 `sameAs` 3)
  print ("parenthesised funlhs", (10 |-| 3) 2, (|-|) 10 3 2)
  print ("backquoted infix funlhs", ((+ 1) `applyTwice`) 5, applyTwice (* 2) 5)
  print ("infix with patterns", [1, 4, 6] `merge` [2, 3, 7])
  print ("prefix operator definition", 3 <+> 4)
  print ("multi-name signature", low, mid, high)
  print ("guards", band 1, band 7, band 20)
  print ("pattern bindings", p1, p2, q1, q2, j1, h1)
  print ("negative literal pattern", isMinusOne (-1), isMinusOne 1)
  print ("guarded pattern binding", gv)
