-- Haskell 2010 Report 9: fst, snd, curry, uncurry
module Main (main) where

addPair :: (Int, Int) -> Int
addPair (a, b) = a + b

sub :: Int -> Int -> Int
sub a b = a - b

main :: IO ()
main = do
  print ("fst", fst (1 :: Int, 'a'), fst ("x", ()), fst ((1 :: Int, 2 :: Int), 'c'))
  print ("snd", snd (1 :: Int, 'a'), snd ("x", ()), snd ('c', (1 :: Int, 2 :: Int)))
  print ("fst/snd lazy in other component", fst (1 :: Int, undefined :: Char), snd (undefined :: Char, 2 :: Int))
  print ("nested", fst (snd (1 :: Int, ('a', True))), snd (fst ((1 :: Int, "in"), 'z')))
  print ("map", map fst [(1 :: Int, 'a'), (2, 'b')], map snd [(1 :: Int, 'a'), (2, 'b')])
  print ("curry", curry addPair 3 4, curry fst 'a' 'b', curry snd 'a' 'b')
  print ("curry partial", map (curry addPair 10) [1, 2, 3])
  print ("curry id", curry id (1 :: Int) 'x')
  print ("uncurry", uncurry sub (10, 3), uncurry (+) (1 :: Int, 2), uncurry max ('a', 'z'))
  print ("uncurry operators", uncurry (:) (1 :: Int, [2, 3]), uncurry (++) ("ab", "cd"), uncurry replicate (3, 'x'))
  print ("uncurry over zip", map (uncurry (*)) (zip [1, 2, 3] [4, 5, 6 :: Int]))
  print ("uncurry const lazy", uncurry const (1 :: Int, undefined :: Char))
  print ("curry . uncurry", curry (uncurry sub) 9 4 == sub 9 4)
  print ("uncurry . curry", uncurry (curry addPair) (9, 4) == addPair (9, 4))
  print ("swap via curry", uncurry (flip (,)) (1 :: Int, 'a'))
  print ("zipWith curry", zipWith (curry fst) "abc" [1, 2, 3 :: Int])
