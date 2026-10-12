{-# LANGUAGE LexicalNegation #-}
-- LexicalNegation: a minus that touches what follows it, and not what comes before, is that thing's sign
module Main (main) where

sign :: Int -> String
sign -1 = "minus one"
sign 0 = "zero"
sign n = if n < 0 then "negative" else "positive"

main :: IO ()
main = do
  let x = 5 :: Int
      xs = [1, 2, 3] :: [Int]
  print (abs -3 :: Int)
  print (map abs [-1, -2, 3 :: Int])
  print (x - 1, x-1, id -1 + x)
  print (-x, negate -x)
  print (-2 ^ (2 :: Int) :: Int)
  print (max -1 -2 :: Int)
  print (map (subtract 1) xs, map (- 1) xs)
  print ((-1) + 2 :: Int, (-x))
  print (map sign [-1, 0, -5, 5])
  print (-1.5 :: Double, -x `mod` 3)
  print (case -x of -5 -> "matched"; _ -> "other")
