-- Haskell 2010 Report 9: RealFloat predicates isNaN isInfinite isDenormalized isIEEE isNegativeZero, and atan2 in all quadrants
module Main (main) where

nan :: Double
nan = 0 / 0

inf :: Double
inf = 1 / 0

near :: Double -> Double -> Bool
near x y = abs (x - y) < 1.0e-12

main :: IO ()
main = do
  print ("isNaN", isNaN nan, isNaN inf, isNaN (0 :: Double), isNaN (inf - inf), isNaN (inf * 0))
  print ("isInfinite", isInfinite inf, isInfinite (negate inf), isInfinite nan, isInfinite (1.0e308 :: Double), isInfinite (1.0e308 * 10 :: Double))
  print ("isNegativeZero", isNegativeZero (-0.0 :: Double), isNegativeZero (0.0 :: Double), isNegativeZero (-1 :: Double), isNegativeZero (0 * (-1) :: Double))
  print ("isDenormalized", isDenormalized (5.0e-324 :: Double), isDenormalized (2.225073858507201e-308 :: Double))
  print ("isDenormalized false", isDenormalized (2.2250738585072014e-308 :: Double), isDenormalized (1 :: Double), isDenormalized (0 :: Double))
  print ("isIEEE", isIEEE (1 :: Double), isIEEE (undefined :: Double))
  print ("show special", inf, negate inf, -0.0 :: Double)
  print ("nan comparisons", nan == nan, nan /= nan, nan < 1, nan > 1, nan <= nan)
  print ("inf comparisons", inf > 1.0e308, negate inf < -1.0e308, inf == inf)
  print ("negative zero", (-0.0 :: Double) == 0.0, 1 / (-0.0 :: Double), recip (0.0 :: Double))
  print ("atan2 axes", atan2 0 1 :: Double, near (atan2 1 0) (pi / 2), near (atan2 0 (-1)) pi, near (atan2 (-1) 0) (-pi / 2))
  print ("atan2 quadrants", near (atan2 1 1) (pi / 4), near (atan2 1 (-1)) (3 * pi / 4), near (atan2 (-1) (-1)) (-3 * pi / 4), near (atan2 (-1) 1) (-pi / 4))
  print ("atan2 origin", atan2 0 0 :: Double)
  print ("atan2 negative zero y", isNegativeZero (atan2 (-0.0) 1 :: Double), near (atan2 (-0.0) (-1)) (negate pi))
  print ("atan2 = atan y/x for x > 0", all (\(y, x) -> near (atan2 y x) (atan (y / x))) [(1, 2), (-3, 4), (0.5, 0.25), (-7, 0.5)])
  print ("atan2 range", all (\(y, x) -> atan2 y x <= pi && atan2 y x > negate pi) [(1, 2), (-3, -4), (0.5, -0.25), (-7, 0.5), (0, -1 :: Double)])
