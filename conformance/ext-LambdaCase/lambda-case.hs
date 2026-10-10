{-# LANGUAGE LambdaCase #-}
-- LambdaCase: \case alternatives is a function of one argument that takes it apart
module Main (main) where

describe :: Maybe Int -> String
describe = \case
  Nothing -> "nothing"
  Just 0 -> "zero"
  Just n
    | n < 0 -> "negative"
    | otherwise -> "positive"

depth :: [[a]] -> Int
depth = \case [] -> 0; (xs : _) -> 1 + length xs

main :: IO ()
main = do
  print (map describe [Nothing, Just 0, Just (-3), Just 7])
  print (depth ["abc", "d"], depth ([] :: [[Int]]))
  print (map (\case True -> 'y'; False -> 'n') [True, False, True])
  print ((\case (a, b) -> a + b) (3 :: Int, 4))
  mapM_ (\case { 1 -> putStrLn "one"; n -> print (n :: Int) }) [1, 2]
  print (foldr (\case 0 -> id; n -> (n :)) [] [0, 1, 0, 2 :: Int])
  let classify = \case
        c | c `elem` "aeiou" -> "vowel"
          | c `elem` ['a' .. 'z'] -> "consonant"
        _ -> "other"
  print (map classify "a b")
  print ((\case x -> \case y -> x - y) (10 :: Int) 4)
