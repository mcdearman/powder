-- Haskell 2010 Report 6.1.4: every implementation must support tuples up to size 15 with Eq, Ord, Bounded, Read, Show
type T15 = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Bool)

type B15 = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

big :: T15
big = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, True)

bigLast :: T15
bigLast = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, False)

bigFirst :: T15
bigFirst = (0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 99, True)

fifteenth :: T15 -> Bool
fifteenth (_, _, _, _, _, _, _, _, _, _, _, _, _, _, x) = x

sumInts :: T15 -> Int
sumInts (a, b, c, d, e, f, g, h, i, j, k, l, m, n, _) = a + b + c + d + e + f + g + h + i + j + k + l + m + n

main :: IO ()
main = do
  print big
  putStrLn (show bigLast)
  print ("eq", big == big, big == bigLast, big /= bigFirst)
  print ("ord", compare big bigLast, compare big bigFirst, big < bigLast, bigFirst < big)
  print ("max/min", max big bigLast == big, min big bigFirst == bigFirst)
  print ("pattern", fifteenth big, fifteenth bigLast, sumInts big)
  print ("read . show", read (show big) == big)
  print ("read", (read "(1,2,3,4,5,6,7,8,9,10,11,12,13,14,False)" :: T15) == bigLast)
  print (minBound :: B15)
  print (maxBound :: B15)
  print ("bounded eq", (minBound :: B15) < maxBound)
