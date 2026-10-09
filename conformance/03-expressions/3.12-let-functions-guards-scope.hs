-- Haskell 2010 Report 3.12: let with local functions, guards, where clauses, nesting and shadowing
module Main (main) where

main :: IO ()
main = do
  let sign :: Int -> Int
      sign n
        | n < 0 = -1
        | n == 0 = 0
        | otherwise = 1
  print ("guards", map sign [-5, 0, 5])
  let collatz :: Int -> Int
      collatz 1 = 0
      collatz n
        | even n = 1 + collatz half
        | otherwise = 1 + collatz (3 * n + 1)
        where
          half = n `div` 2
  print ("clauses, guards, where", map collatz [1, 2, 3, 6, 7])
  print ("guarded pattern binding", let v | 1 > (2 :: Int) = "a" | otherwise = "b" in v)
  print ("empty let", let {} in 5)
  print ("explicit braces", let { a = 1; b = 2 } in a + b)
  print ("extra semicolons", let { ; a = 1; ; b = 2; } in a + b)
  print ("body extends right", let x = 2 in x * x + 1)
  print ("parenthesised", (let x = 2 in x * x) + 1)
  print ("as right operand", 10 + let x = 2 in x * x)
  print ("shadowing", let x = 1 in let x = 2 in x)
  print ("shadow parameter", (\x -> let x = 'b' in x) 'a')
  print ("shadow then outer", let x = 1 in (let x = 2 in x) + x)
  print ("function and value", let { sq n = n * n; four = sq 2 } in sq four)
  print ("local operator", let a |> f = f a in 3 |> (+ 1) |> (* 2))
  print ("infix definition", let a `plus` b = a + b in 3 `plus` 4)
  print ("let in lambda", map (\n -> let m = n * n in m + m) [1, 2, 3])
  print ("let in guard", pick 5, pick 50)
  print ("let in if", if let ok = True in ok then "yes" else "no")
  print ("signature in let", let { n :: Int; n = maxBound } in n == maxBound)
  print ("multiline", multi)
  where
    pick :: Int -> String
    pick n
      | let m = n * 2, m > 20 = "big"
      | otherwise = "small"
    multi :: Int
    multi =
      let a = 1
          b = a + 1
          c = b + 1
       in a + b + c
