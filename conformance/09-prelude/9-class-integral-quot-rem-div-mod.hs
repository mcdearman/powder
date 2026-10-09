-- Haskell 2010 Report 9: class Integral (quot rem div mod quotRem divMod toInteger) at Int and Integer, all sign combinations
module Main (main) where

pairs :: [(Int, Int)]
pairs = [(7, 2), (-7, 2), (7, -2), (-7, -2), (6, 3), (-6, 3), (6, -3), (0, 5), (0, -5), (1, 5), (-1, 5), (1, -5), (5, 1), (5, -1)]

row :: (Int, Int) -> IO ()
row (a, b) = print ((a, b), a `quot` b, a `rem` b, a `div` b, a `mod` b, quotRem a b, divMod a b)

rowI :: (Integer, Integer) -> IO ()
rowI (a, b) = print ((a, b), a `quot` b, a `rem` b, a `div` b, a `mod` b, quotRem a b, divMod a b)

lawQuotRem :: (Int, Int) -> Bool
lawQuotRem (a, b) = (a `quot` b) * b + (a `rem` b) == a

lawDivMod :: (Int, Int) -> Bool
lawDivMod (a, b) = (a `div` b) * b + (a `mod` b) == a

big :: Integer
big = 10 ^ (30 :: Int)

main :: IO ()
main = do
  mapM_ row pairs
  mapM_ rowI [(big + 7, 1000), (negate big - 7, 1000), (big + 7, -1000), (negate big - 7, -1000), (big, big), (7, big), (-7, big)]
  print ("law quotRem", all lawQuotRem pairs)
  print ("law divMod", all lawDivMod pairs)
  print ("rem sign follows dividend", map (\(a, b) -> signum (a `rem` b)) (take 4 pairs))
  print ("mod sign follows divisor", map (\(a, b) -> signum (a `mod` b)) (take 4 pairs))
  print ("prefix forms", quot 17 5 :: Int, rem 17 5 :: Int, div 17 5 :: Int, mod 17 5 :: Int)
  print ("Int extremes", (maxBound :: Int) `div` 2, (minBound :: Int) `div` 2, (minBound :: Int) `mod` 10, (minBound :: Int) `rem` 10)
  print ("Int extremes quot", (minBound :: Int) `quot` 2, (maxBound :: Int) `quotRem` (-1), (minBound :: Int) `divMod` 3)
  print ("toInteger", toInteger (5 :: Int), toInteger (-5 :: Int), toInteger (maxBound :: Int) + 1, toInteger (7 :: Integer))
  print ("toInteger minBound", toInteger (minBound :: Int) - 1)
  print ("sections", map (`div` 2) [-3 .. 3 :: Int], map (`mod` 3) [-3 .. 3 :: Int])
  print ("sections quot/rem", map (`quot` 2) [-3 .. 3 :: Int], map (`rem` 3) [-3 .. 3 :: Int])
