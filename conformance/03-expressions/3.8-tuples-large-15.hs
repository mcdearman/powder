-- Haskell 2010 Report 3.8 / 6.1.4: tuples of any size; Eq, Ord and Show instances for sizes up to 15
module Main (main) where

type T15 = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

t1 :: T15
t1 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

t2 :: T15
t2 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16)

t3 :: T15
t3 = (0, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99)

first15 :: T15 -> Int
first15 (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a

last15 :: T15 -> Int
last15 (_, _, _, _, _, _, _, _, _, _, _, _, _, _, o) = o

sum15 :: T15 -> Int
sum15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = a + b + c + d + e + f + g + h + i + j + k + l + m + n + o

mixed :: (Int, Char, Bool, String, Double, Integer, (), Maybe Int, [Int], Ordering, (Int, Int), Either Int Char, Float, Char, Int)
mixed = (1, 'a', True, "s", 1.5, 2, (), Just 3, [4], LT, (5, 6), Left 7, 0.5, 'z', 8)

sum16 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Int
sum16 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p

main :: IO ()
main = do
  print t1
  print mixed
  print ("show", show t1 == "(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)")
  print ("eq", t1 == t1, t1 == t2, t1 /= t2)
  print ("ord last", t1 < t2, compare t2 t1, max t1 t2 == t2)
  print ("ord first", t3 < t1, compare t1 t3, minimum [t1, t2, t3] == t3)
  print ("select", first15 t1, last15 t2, sum15 t1)
  print ("mixed eq", mixed == mixed, compare mixed mixed)
  print ("size 8", (1, 2, 3, 4, 5, 6, 7, 8))
  print ("size 12", (1, 'b', 3, 'd', 5, 'f', 7, 'h', 9, 'j', 11, 'l'))
  print ("size 8 ord", (1, 2, 3, 4, 5, 6, 7, 8) < (1, 2, 3, 4, 5, 6, 7, 9))
  print ("size 16 by pattern", sum16 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
