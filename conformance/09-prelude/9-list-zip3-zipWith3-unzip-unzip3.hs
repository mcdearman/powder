-- Haskell 2010 Report 9 (PreludeList): zip3, zipWith3, unzip, unzip3
module Main (main) where

triples :: [(Int, Char, Bool)]
triples = [(1, 'a', True), (2, 'b', False), (3, 'c', True)]

main :: IO ()
main = do
  print ("zip3", zip3 [1, 2, 3 :: Int] "abc" [True, False, True])
  print ("zip3 first shortest", zip3 [1 :: Int] "abc" [True, False, True])
  print ("zip3 second shortest", zip3 [1, 2, 3 :: Int] "ab" [True, False, True])
  print ("zip3 third shortest", zip3 [1, 2, 3 :: Int] "abc" [True])
  print ("zip3 empty", zip3 ([] :: [Int]) "abc" [True], zip3 [1 :: Int] "" [True], zip3 [1 :: Int] "a" ([] :: [Bool]))
  print ("zip3 infinite", zip3 [1 :: Int ..] "ab" (repeat ()), take 2 (zip3 [1 :: Int ..] (cycle "xy") [10 :: Int ..]))
  print ("zip3 lazy in elements", length (zip3 [undefined :: Int] "a" [undefined :: Bool]))
  print ("zipWith3", zipWith3 (\a b c -> a + b + c) [1, 2, 3 :: Int] [10, 20, 30] [100, 200, 300])
  print ("zipWith3 unequal", zipWith3 (\a b c -> a + b + c) [1, 2, 3 :: Int] [10, 20] [100, 200, 300, 400])
  print ("zipWith3 empty", zipWith3 (\a b c -> a + b + c) ([] :: [Int]) [1] [2], zipWith3 (,,) "a" "" "c")
  print ("zipWith3 different types", zipWith3 (\n c b -> if b then replicate n c else "") [1, 2, 3] "abc" [True, False, True])
  print ("zipWith3 infinite", zipWith3 (\a b c -> a * b + c) [1 :: Int ..] [2 ..] [0, 0, 1])
  print ("zipWith3 constructor", zipWith3 (,,) [1, 2 :: Int] "ab" [(), ()])
  print ("zip3 = zipWith3 (,,)", zip3 "ab" "cd" "ef" == zipWith3 (,,) "ab" "cd" "ef")
  print ("unzip", unzip [(1 :: Int, 'a'), (2, 'b'), (3, 'c')])
  print ("unzip empty", unzip ([] :: [(Int, Char)]))
  print ("unzip singleton", unzip [((), "only")])
  print ("unzip . zip", unzip (zip [1, 2, 3 :: Int] "abc") == ([1, 2, 3], "abc"), unzip (zip [1, 2, 3 :: Int] "ab"))
  print ("unzip lazy", take 3 (fst (unzip (zip [1 :: Int ..] [1 :: Int ..]))), take 2 (snd (unzip (map (\n -> (n, n * n)) [1 :: Int ..]))))
  print ("unzip lazy in components", length (fst (unzip [(undefined :: Int, 'a'), (undefined, 'b')])), snd (unzip [(undefined :: Int, 'a')]))
  print ("unzip nested", unzip [((1 :: Int, 'a'), [True]), ((2, 'b'), [])])
  print ("unzip3", unzip3 triples)
  print ("unzip3 empty", unzip3 ([] :: [(Int, Char, Bool)]))
  print ("unzip3 singleton", unzip3 [('x', "y", 0.5 :: Double)])
  print ("unzip3 . zip3", (\(a, b, c) -> zip3 a b c) (unzip3 triples) == triples)
  print ("unzip3 lazy", (\(a, _, _) -> take 2 a) (unzip3 (zip3 [1 :: Int ..] (repeat 'x') (repeat ()))))
  print ("unzip3 components", (\(_, b, _) -> b) (unzip3 triples), (\(_, _, c) -> and c) (unzip3 triples))
