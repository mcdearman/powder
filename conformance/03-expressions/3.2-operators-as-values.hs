-- Haskell 2010 Report 3.2: an operator in parentheses is an ordinary value
module Main (main) where

apply2 :: (a -> b -> c) -> a -> b -> c
apply2 f x y = f x y

(|+|) :: Int -> Int -> Int
a |+| b = a * 10 + b

comb :: (a -> a -> a) -> a -> a -> a
comb (%%) a b = (a %% b) %% a

main :: IO ()
main = do
  print ("plus", (+) 1 2)
  print ("minus", (-) 10 3)
  print ("fold", foldr (+) 0 [1, 2, 3, 4])
  print ("zipWith", zipWith (*) [1, 2, 3] [4, 5, 6])
  print ("cons", (:) 1 [2, 3])
  print ("append", (++) "ab" "cd")
  print ("compose", ((.) (+ 1) (* 2)) 5)
  print ("dollar", ($) negate 3)
  print ("eq", (==) 'a' 'a', (/=) 'a' 'a')
  print ("ord", (<) 1 2, (>=) 1 2)
  print ("bool", (&&) True False, (||) True False)
  print ("user", (|+|) 4 2, 4 |+| 2, apply2 (|+|) 1 2)
  print ("index", (!!) "hello" 1)
  print ("uncurry", map (uncurry (-)) [(5, 1), (9, 4)])
  print ("flip", flip (-) 1 10)
  print ("pow", (^) 2 10, (**) 2.0 3.0)
  print ("let op", let (<+>) = (+) in 3 <+> 4)
  print ("op parameter", comb (+) 1 2, comb (++) "a" "b")
  print ("partial", map ((+) 10) [1, 2], map ((-) 10) [1, 2])
  print ("spaces", ( + ) 1 2, ( |+| ) 1 2)
  print ("equal forms", (+) 3 4 == 3 + 4, (|+|) 3 4 == 3 |+| 4)
