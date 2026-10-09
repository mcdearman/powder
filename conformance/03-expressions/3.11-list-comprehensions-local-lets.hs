-- Haskell 2010 Report 3.11: local let declarations inside list comprehensions
module Main (main) where

main :: IO ()
main = do
  print ("let", [(x, y) | x <- [1 .. 3], let y = x * x])
  print ("let two bindings", [(a, b) | x <- [1 .. 3], let { a = x + 1; b = x * 10 }])
  print ("let then guard", [y | x <- [1 .. 6], let y = x * x, even y])
  print ("let then generator", [z | x <- [1, 2], let ys = [x, x * 10], z <- ys])
  print ("let function", [f x | x <- [1 .. 3], let f n = n * n + 1])
  print ("let polymorphic", [(pair x, pair 'c') | x <- [1, 2], let pair v = (v, v)])
  print ("let pattern", [q + r | x <- [7, 8, 9], let (q, r) = x `divMod` 4])
  print ("let first", [y | let y = 5])
  print ("let first then generator", [x + k | let k = 100, x <- [1, 2]])
  print ("let only with guard", [z | let z = 1, True], [z | let z = 1, False])
  print ("two lets", [c | x <- [1, 2], let a = x + 1, let b = a * 2, let c = a + b])
  print ("let recursive", [take 3 xs | x <- [1, 2], let xs = x : xs])
  print ("let mutually recursive", [(ev 4, od 4) | let { ev 0 = True; ev n = od (n - 1); od 0 = False; od n = ev (n - 1) }])
  print ("let unused lazy", [x | x <- [1, 2], let _unused = undefined :: Int])
  print ("let with signature", [y | x <- [1, 2], let { y :: Double; y = fromIntegral x / 2 }])
  print ("let guards", [s | x <- [-1, 0, 1], let s | x < 0 = "neg" | x == 0 = "zero" | otherwise = "pos"])
