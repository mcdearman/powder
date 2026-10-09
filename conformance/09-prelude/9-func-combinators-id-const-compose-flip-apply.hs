-- Haskell 2010 Report 9: id, const, (.), flip, ($)
module Main (main) where

inc :: Int -> Int
inc = (+ 1)

dbl :: Int -> Int
dbl = (* 2)

main :: IO ()
main = do
  print ("id", id (3 :: Int), id 'x', id "s", map id [1, 2 :: Int])
  print ("id on functions", id inc 1, (id . id) 'q', id id True)
  print ("const", const (1 :: Int) 'x', const 'a' (2 :: Int), map (const 0) "abc" :: [Int])
  print ("const lazy", const (1 :: Int) (undefined :: Bool), const () (undefined :: Int))
  print ("const const", const const (1 :: Int) 'a' True, const id (1 :: Int) 'k')
  print ("compose", (inc . dbl) 5, (dbl . inc) 5, (inc . inc . inc) 0)
  print ("compose types", (show . inc) 9, (length . show) (12345 :: Int), (not . null) "x")
  print ("compose prefix", (.) inc dbl 5, (.) show length "abc")
  print ("compose sections", (inc .) dbl 5, (. dbl) inc 5)
  print ("compose with id", (id . inc) 1 == inc 1, (inc . id) 1 == inc 1)
  print ("compose lazy", (const 7 . (undefined :: Int -> Int)) 1 :: Int)
  print ("compose list", foldr (.) id [inc, dbl, inc] 1, foldr (.) id [] 'z')
  print ("flip", flip (-) 1 (10 :: Int), flip div 2 (10 :: Int), flip (:) [2, 3] (1 :: Int))
  print ("flip const", flip const (undefined :: Int) 'y', flip (,) 'a' True)
  print ("flip flip", flip (flip (-)) 10 (1 :: Int), flip replicate 'x' 3)
  print ("flip map", flip map [1, 2, 3] inc, flip elem "abc" 'b')
  print ("apply", inc $ 5, dbl $ inc $ 5, show $ 1 + (2 :: Int))
  print ("apply prefix", ($) inc 5, map ($ 3) [inc, dbl, negate])
  print ("apply section", zipWith ($) [inc, dbl] [10, 20], (inc $) 1)
  print ("apply is right associative", negate $ abs $ inc $ -10)
  print ("apply with compose", inc . dbl $ 4, map (inc . dbl) $ [1, 2] ++ [3])
