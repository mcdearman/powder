-- Haskell 2010 Report 6.4: Int is fixed precision; this suite assumes 64-bit two's complement arithmetic that wraps
-- (the Report leaves overflow undefined; the suite's brief fixes wraparound). fromInteger/fromIntegral reduce modulo 2^64.
maxI :: Int
maxI = maxBound

minI :: Int
minI = minBound

two64 :: Integer
two64 = 2 ^ (64 :: Int)

main :: IO ()
main = do
  print ("bounds", minI, maxI)
  print ("maxBound + 1", maxI + 1, maxI + 1 == minI)
  print ("minBound - 1", minI - 1, minI - 1 == maxI)
  print ("maxBound + maxBound", maxI + maxI)
  print ("minBound + minBound", minI + minI)
  print ("maxBound * 2", maxI * 2)
  print ("maxBound * maxBound", maxI * maxI)
  print ("2^62 * 2, 2^62 * 4", 2 ^ (62 :: Int) * 2 :: Int, 2 ^ (62 :: Int) * 4 :: Int)
  print ("2^63, 2^64, 2^65 at Int", 2 ^ (63 :: Int) :: Int, 2 ^ (64 :: Int) :: Int, 2 ^ (65 :: Int) :: Int)
  print ("3^41 wraps", 3 ^ (41 :: Int) :: Int, (3 ^ (41 :: Int) :: Integer) `mod` two64)
  print ("succ-free increment near bound", maxI - 1 + 1 == maxI, minI + 1 - 1 == minI)
  print ("fromInteger in range", fromInteger 42 :: Int, fromInteger (-42) :: Int, fromInteger (2 ^ (63 :: Int) - 1) :: Int)
  print ("fromInteger 2^63", fromInteger (2 ^ (63 :: Int)) :: Int)
  print ("fromInteger 2^64 + 5", fromInteger (two64 + 5) :: Int)
  print ("fromInteger -(2^63) - 1", fromInteger (negate (2 ^ (63 :: Int)) - 1) :: Int)
  print ("fromInteger 2^100 + 7", fromInteger (2 ^ (100 :: Int) + 7) :: Int)
  print ("fromIntegral Integer -> Int", fromIntegral (two64 - 1) :: Int, fromIntegral (3 * two64 + 9) :: Int)
  print ("toInteger does not wrap", toInteger maxI + 1, toInteger minI - 1)
  print ("toInteger . fromInteger", toInteger (fromInteger (two64 + 123) :: Int))
  print ("wrapped sum equals sum mod 2^64", toInteger (maxI + 10) == (toInteger maxI + 10) - two64)
  print ("hash-style arithmetic", foldl (\h c -> h * 31 + fromEnum c) 7 (concat (replicate 20 "conformance")))
