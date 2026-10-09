-- Haskell 2010 Report 4.4.3.2: pattern bindings with guards and where; nested and irrefutable (~) patterns
module Main (main) where

flag :: Bool
flag = True

lo, hi :: Int
(lo, hi)
  | flag = (1, 2)
  | otherwise = (3, 4)

-- a simple pattern binding with guards
threshold :: Int
threshold
  | flag, lo < hi = 10
  | otherwise = 20

-- guards and a where clause scoping over all of them
small, large :: Int
(small, large)
  | a < b = (a, b)
  | otherwise = (b, a)
  where
    a = 9
    b = 4

total :: Int
total = base + extra
  where
    base = 40
    extra = 2

n1, n2, n3, n4 :: Int
(n1, (n2, [n3, n4])) = (1, (2, [3, 4]))

-- the inner ~ keeps the undefined component from being matched when `outerOnly` is demanded
outerOnly :: Int
outerOnly = let (a, ~(_, _)) = (1, undefined :: (Int, Int)) in a

explicitTilde :: Int
explicitTilde = let ~(a, b) = (20, 22) in a + b

wildcards :: Char
wildcards = let (_, c, _) = (undefined :: Int, 'c', undefined :: Bool) in c

asNested :: ((Int, Char), Int)
asNested = let whole@(h, _) = (5, 'x') in (whole, h)

localGuards :: Int -> String
localGuards n = verdict
  where
    (verdict, _)
      | n > 0 = ("positive", n)
      | n < 0 = ("negative", negate n)
      | otherwise = ("zero", 0)

main :: IO ()
main = do
  print ("guarded tuple", lo, hi)
  print ("guarded simple", threshold)
  print ("guards with where", small, large)
  print ("where on simple binding", total)
  print ("nested", n1, n2, n3, n4)
  print ("nested irrefutable", outerOnly)
  print ("explicit tilde", explicitTilde)
  print ("wildcards", wildcards)
  print ("as-pattern", asNested)
  print ("local guarded pattern", map localGuards [5, -5, 0])
