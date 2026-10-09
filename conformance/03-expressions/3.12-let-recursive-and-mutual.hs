-- Haskell 2010 Report 3.12: let bindings are mutually recursive and their order is irrelevant
module Main (main) where

main :: IO ()
main = do
  let fact :: Integer -> Integer
      fact n = if n <= 1 then 1 else n * fact (n - 1)
  print ("recursive", fact 20)
  let isEven, isOdd :: Int -> Bool
      isEven 0 = True
      isEven n = isOdd (n - 1)
      isOdd 0 = False
      isOdd n = isEven (n - 1)
  print ("mutual", isEven 10, isOdd 7, isEven 3)
  print ("order irrelevant", let { a = b + 1; b = c * 2; c = 5 } in (a, b, c))
  print ("cyclic list", let xs = 1 : 2 : xs in take 5 xs)
  print ("mutual values", let { ys = 0 : zs; zs = map (+ 1) ys } in take 4 zs)
  print ("fibs", let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in take 10 fibs)
  print ("recursive pattern binding", let (evens, odds) = (0 : map (+ 1) odds, map (+ 1) evens) in (take 3 evens, take 3 odds))
  print ("loop", let { go acc [] = acc; go acc (y : ys) = go (acc + y) ys } in go 0 [1 .. 10])
  print ("function before its helper", let { f n = g n + 1; g n = n * 2 } in f 5)
  print ("three-way", let { p n = if n == 0 then "p" else q (n - 1); q n = if n == 0 then "q" else r (n - 1); r n = if n == 0 then "r" else p (n - 1) } in map p [0, 1, 2, 3, 4])
  print ("nested let", let a = 1 in let b = a + 1 in let c = a + b in (a, b, c))
  print ("inner refers outward and to itself", let n = 3 in let count k = if k >= n then [k] else k : count (k + 1) in count 0)
  print ("ackermann", let { ack 0 n = n + 1; ack m 0 = ack (m - 1) 1; ack m n = ack (m - 1) (ack m (n - 1)) } in ack 2 3)
