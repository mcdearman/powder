-- Haskell 2010 Report 9 (PreludeList): foldr and the functions defined with it are lazy: they stop early on infinite lists
module Main (main) where

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr pick Nothing
  where
    pick (Just x) _ = Just x
    pick Nothing rest = rest

main :: IO ()
main = do
  print ("foldr ignoring accumulator", foldr (\x _ -> x) 0 [1 :: Int ..])
  print ("foldr && on infinite", foldr (\x acc -> x < 10 && acc) True [1 :: Int ..])
  print ("foldr || on infinite", foldr (\x acc -> x > 10 || acc) False [1 :: Int ..])
  print ("foldr producing a list lazily", take 3 (foldr (\x acc -> x * 2 : acc) [] [1 :: Int ..]))
  print ("foldr takeWhile", take 5 (foldr (\x acc -> if x < 4 then x : acc else []) [] [1 :: Int ..]))
  print ("foldr with undefined tail", foldr (\x _ -> x) 'z' ('a' : undefined))
  print ("foldr firstJust", firstJust (Nothing : Nothing : Just 'k' : undefined), firstJust (map (\n -> if n > 5 then Just n else Nothing) [1 :: Int ..]))
  print ("and cycle", and (cycle [False]), and (cycle [True, False]))
  print ("and infinite map", and (map (< 5) [1 :: Int ..]))
  print ("and undefined after False", and [True, False, undefined], and (False : undefined))
  print ("or cycle", or (cycle [True]), or (cycle [False, True]))
  print ("or infinite map", or (map (> 100) [1 :: Int ..]))
  print ("or undefined after True", or [False, True, undefined], or (True : undefined))
  print ("any infinite", any even [1 :: Int ..], any (> 1000) (iterate (* 2) (1 :: Int)))
  print ("any undefined after hit", any (== 'b') ('a' : 'b' : undefined))
  print ("all infinite", all odd [1 :: Int ..], all (< 1000) (iterate (* 2) (1 :: Int)))
  print ("all undefined after miss", all (== 'a') ('a' : 'b' : undefined))
  print ("elem infinite", 5 `elem` [1 :: Int ..], 'c' `elem` cycle "abc")
  print ("notElem infinite", 5 `notElem` [1 :: Int ..])
  print ("elem undefined after hit", 2 `elem` (1 : 2 : undefined :: [Int]))
  print ("lookup infinite", lookup 3 (zip [1 :: Int ..] (cycle "abc")), lookup 1 ((1 :: Int, 'a') : undefined))
  print ("concat infinite", take 4 (concat (map (\x -> [x, x]) [1 :: Int ..])))
  print ("takeWhile infinite", takeWhile (< 4) [1 :: Int ..], takeWhile (< 3) (1 : 2 : 3 : undefined :: [Int]))
  print ("head filter infinite", head (filter (> 100) (map (^ (2 :: Int)) [1 :: Int ..])))
  print ("zip infinite", zip [1 :: Int ..] "abc", take 2 (zip [1 :: Int ..] [10 :: Int ..]))
  print ("unzip lazily", take 2 (fst (unzip (zip [1 :: Int ..] (repeat 'x')))))
  print ("map then head", head (map (* 2) [1 :: Int ..]), null (map undefined [1 :: Int ..] :: [Int]))
