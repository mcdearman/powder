-- The numeric classes: doubles, ratios, and going between them.

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

newton :: Double -> Double
newton x = until (\g -> abs (g * g - x) < 1e-12) (\g -> (g + x / g) / 2) 1

half :: Rational
half = 1 / 2

main :: IO ()
main = do
  print (1.5 + 2, 7 / 2, 2 ** 10, sqrt 2)
  print (mean [1, 2, 3, 4], newton 2 == sqrt 2 || abs (newton 2 - sqrt 2) < 1e-9)
  print (truncate (-2.7 :: Double) :: Int, round 2.5 :: Int, round 3.5 :: Int, ceiling 2.1 :: Int, floor (-2.1) :: Int)
  print (half + 1 / 3, toRational (0.75 :: Double), fromRational (3 / 8) :: Double)
  print (fromIntegral (3 :: Int) * 0.5, realToFrac (1.25 :: Float) :: Double)
  print ([0.5, 1 .. 3], [1.0 .. 4], 2 ^^ negate 2)
  print (1 / 0, isNaN (0 / 0), -0.1, 1.0e-2, 12345678.9, 1e22, pi)
  print (minimum [3.5, 2.25, 9], show 2.0, floor (logBase 2 1024 + 0.5) :: Integer)
