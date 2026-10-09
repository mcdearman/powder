-- Haskell 2010 Report 9 (PreludeList): foldl, foldl1, foldr, foldr1; association direction and base cases
module Main (main) where

paren :: String -> String -> String
paren a b = "(" ++ a ++ "." ++ b ++ ")"

main :: IO ()
main = do
  print ("foldl shape", foldl paren "z" ["a", "b", "c"])
  print ("foldr shape", foldr paren "z" ["a", "b", "c"])
  print ("foldl1 shape", foldl1 paren ["a", "b", "c"])
  print ("foldr1 shape", foldr1 paren ["a", "b", "c"])
  print ("empty", foldl paren "z" [], foldr paren "z" [])
  print ("singleton", foldl paren "z" ["a"], foldr paren "z" ["a"], foldl1 paren ["a"], foldr1 paren ["a"])
  print ("foldl (-)", foldl (-) 10 [1, 2, 3 :: Int], foldl (-) 0 ([] :: [Int]))
  print ("foldr (-)", foldr (-) 10 [1, 2, 3 :: Int], foldr (-) 0 ([] :: [Int]))
  print ("foldl1 (-)", foldl1 (-) [10, 1, 2, 3 :: Int], foldl1 (-) [5 :: Int])
  print ("foldr1 (-)", foldr1 (-) [10, 1, 2, 3 :: Int], foldr1 (-) [5 :: Int])
  print ("foldl (/)", foldl (/) 64 [2, 4, 8 :: Double], foldr (/) 2 [64, 8 :: Double])
  print ("foldl different types", foldl (\n c -> n * 10 + fromEnum c - 48) 0 "1234", foldl (\acc x -> x : acc) [] "abc")
  print ("foldr different types", foldr (\c n -> n + fromEnum c - 96) 0 "abc", foldr (\x acc -> acc ++ [x]) [] "abc")
  print ("foldr builds list", foldr (:) [] "same", foldr (\x acc -> x : x : acc) [] [1, 2 :: Int])
  print ("foldl flip (:) reverses", foldl (flip (:)) [] [1, 2, 3 :: Int])
  print ("foldr as map/filter", foldr ((:) . (* 2)) [] [1, 2, 3 :: Int], foldr (\x acc -> if even x then x : acc else acc) [] [1 .. 6 :: Int])
  print ("foldl1/foldr1 max", foldl1 max [3, 1, 4, 1, 5 :: Int], foldr1 min [3, 1, 4, 1, 5 :: Int])
  print ("foldr1 lazy", foldr1 (\x _ -> x) [1 :: Int ..], foldr1 (||) (True : cycle [False]))
  print ("foldl large", foldl (+) 0 [1 .. 100000 :: Int])
  print ("foldr large", foldr (+) 0 [1 .. 100000 :: Int])
  print ("foldl const", foldl const 0 [1, 2, 3 :: Int], foldl (flip const) 0 [1, 2, 3 :: Int])
  print ("foldl ignores undefined elements if f does", foldl (\n _ -> n + 1) (0 :: Int) [undefined, undefined :: Char])
  print ("fold of folds", foldr (\row acc -> foldl (+) 0 row : acc) [] [[1, 2], [3], [] :: [Int]])
