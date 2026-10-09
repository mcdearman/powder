-- Haskell 2010 Report 3.13: variables bound by a case alternative scope over that alternative only and shadow outer bindings
module Main (main) where

h :: Int -> Int
h x = case x + 1 of
  x -> x * 2

swapTwice :: (Int, Int) -> (Int, Int)
swapTwice p = case p of
  (a, b) -> case (b, a) of
    (a, b) -> (a * 10, b)

outer :: Int
outer = 7

main :: IO ()
main = do
  let y = 10 :: Int
  print ("shadow parameter", h 1)
  print ("shadow let", case 5 of { y -> y })
  print ("let unchanged", y)
  print ("nested shadow", swapTwice (1, 2))
  print ("scrutinee uses outer v", let v = 3 in case v + 1 of { v -> v * 2 })
  print ("shadow top-level", case 1 of { outer -> outer }, outer)
  print ("alternative-local", case Just 1 of { Just n -> n; Nothing -> outer })
  print ("each alternative own vars", [case e of { Left n -> n + 1; Right n -> length n } | e <- [Left 1, Right "abc"]])
  print ("guard sees outer and bound", case 4 of { n | n > y -> "bigger"; n | n + outer > y -> "with outer"; _ -> "no" })
  print ("shadow prelude name", case [1, 2, 3] of { (head : tail) -> (head, tail); [] -> (0, []) })
  print ("lambda in alternative", (case 2 of { n -> \m -> n + m }) 3)
  print ("case in case scrutinee", case (case 1 of { 1 -> 'a'; _ -> 'b' }) of { 'a' -> "inner a"; _ -> "inner other" })
  print ("wildcard binds nothing", case 9 of { _ -> y })
