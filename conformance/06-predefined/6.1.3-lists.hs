-- Haskell 2010 Report 6.1.3: lists; [] and (:), list syntax as sugar, Eq/Ord/Show/Read on lists, laziness of (:)
total :: [Int] -> Int
total [] = 0
total (x : xs) = x + total xs

second :: [a] -> Maybe a
second (_ : y : _) = Just y
second _ = Nothing

main :: IO ()
main = do
  print ("nil", [] :: [Int])
  print ("cons chain", 1 : 2 : 3 : [] :: [Int])
  print ("sugar", [1, 2, 3] == (1 : (2 : (3 : []))) && [1, 2, 3] == (1 : [2, 3 :: Int]))
  print ("prefix cons", (:) 'a' "bc", (:) (1 :: Int) [])
  print ("nested", [[1], [], [2, 3]] :: [[Int]])
  print ("patterns", total [1, 2, 3], second "ab", second [True])
  print ("eq", [1, 2] == [1, 2 :: Int], [1, 2] /= [1, 2, 3 :: Int], ([] :: [Int]) == [])
  print ("ord prefix", [1, 2] < [1, 2, 3 :: Int], ([] :: [Int]) < [0], [2] > [1, 9, 9 :: Int])
  print ("ord compare", compare [1, 2, 3] [1, 3 :: Int], compare [3] [3 :: Int], compare [1] ([] :: [Int]))
  print ("max/min", max [1, 2] [1, 3 :: Int], min "abc" "ab")
  print ("show", show [1, 2, 3 :: Int], show ([] :: [Bool]), show [[()]], show [Just 'a'])
  print ("read", read "[1,2,3]" :: [Int], read " [ ] " :: [Int], read "[[1],[]]" :: [[Int]])
  print ("append", [1, 2] ++ [3 :: Int], [] ++ "x", "x" ++ [])
  print ("units", [(), ()], length [(), (), ()])
  print ("functions in a list", map (\f -> f 10) [(+ 1), (* 2), subtract 3 :: Int -> Int])
  print ("lazy tail", take 3 (1 : 2 : 3 : undefined :: [Int]))
  print ("lazy elements", length [undefined, undefined :: Int])
  print ("infinite", take 4 (let ones = 1 : ones in ones :: [Int]))
  print ("functor/monad", fmap (+ 1) [1, 2 :: Int], [1, 2 :: Int] >>= \x -> [x, x * 10])
  print ("arithmetic sequence", [1 .. 5 :: Int], [1, 3 .. 9 :: Int])
  print ("comprehension", [x * x | x <- [1 .. 5 :: Int], odd x])
