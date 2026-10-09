-- Haskell 2010 Report 4.5.4: lambda-, case- and argument-bound variables are monomorphic; let-bound ones are generalised
module Main (main) where

-- f is used at a single type inside the body
applyBoth :: (Int -> Int) -> (Int, Int)
applyBoth f = (f 1, f 2)

-- inferred: (a -> a) -> a -> a; the argument is monomorphic within one call,
-- but different calls may pick different types
selfCompose f x = f (f x)

-- a mutually recursive group: within the group each member is used at one type,
-- but the group as a whole is generalised afterwards
ping x n = if n == 0 then [x] else x : pong x (n - 1)

pong x n = if n == 0 then [] else ping x (n - 1)

main :: IO ()
main = do
  print ("lambda-bound at one type", (\i -> (i 1, i (2 :: Int))) id)
  print ("let-bound at two types", let i = id in (i 'a', i True))
  print ("case-bound at one type", case (id, 'x') of (g, c) -> (g c, g 'y'))
  print ("argument at one type", applyBoth (+ 10), applyBoth negate)
  print ("different calls different types", selfCompose not True, selfCompose (+ 1) (0 :: Int), selfCompose tail "abc")
  print ("polymorphic argument, one type per call", (\k -> k 'a') id, (\k -> k True) id)
  print ("lambda-bound number at one type", (\n -> (n + 1, n * 2)) (5 :: Int))
  print ("group used at Char", ping 'a' (3 :: Int))
  print ("group used at Bool", ping True (2 :: Int), pong False (3 :: Int))
  print ("generator-bound at one type", [f 1 + f 2 | f <- [(+ 1), (* 2)]] :: [Int])
