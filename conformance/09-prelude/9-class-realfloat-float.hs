-- Haskell 2010 Report 9: class RealFloat at Float (IEEE single: radix 2, 24 digits, range (-125,128))
module Main (main) where

samples :: [Float]
samples = [1, 0.5, 8, -1.5, 0.1, 12345.678, 1.0e30, 1.0e-30, -0.75, 3]

roundTrip :: Float -> Bool
roundTrip x = uncurry encodeFloat (decodeFloat x) == x

mantissaInRange :: Float -> Bool
mantissaInRange x = lo <= abs m && abs m < 2 * lo
  where
    (m, _) = decodeFloat x
    lo = 2 ^ (23 :: Int)

main :: IO ()
main = do
  print ("floatRadix", floatRadix (1 :: Float))
  print ("floatDigits", floatDigits (1 :: Float))
  print ("floatRange", floatRange (1 :: Float))
  print ("decodeFloat 1", decodeFloat (1 :: Float))
  print ("decodeFloat 0.5", decodeFloat (0.5 :: Float))
  print ("decodeFloat 0", decodeFloat (0 :: Float))
  print ("decodeFloat -1.5", decodeFloat (-1.5 :: Float))
  print ("decodeFloat 0.1", decodeFloat (0.1 :: Float))
  print ("decodeFloat max", decodeFloat (3.4028235e38 :: Float))
  print ("encodeFloat", encodeFloat 1 10 :: Float, encodeFloat 3 (-1) :: Float, encodeFloat 0 7 :: Float, encodeFloat (-5) 2 :: Float)
  print ("round trip", all roundTrip samples, roundTrip 0)
  print ("mantissa range", all mantissaInRange samples)
  print ("exponent", map exponent [8, 1, 0.5, 0.1, 0, -8 :: Float])
  print ("significand", map significand [8, 1, 0.5, 0.75, 0, -8, 3 :: Float])
  print ("scaleFloat", scaleFloat 3 (1.5 :: Float), scaleFloat (-1) (1 :: Float), scaleFloat 5 (0 :: Float))
  print ("predicates", isNaN (0 / 0 :: Float), isInfinite (1 / 0 :: Float), isNegativeZero (-0.0 :: Float), isIEEE (1 :: Float))
  print ("predicates false", isNaN (1 :: Float), isInfinite (1 :: Float), isNegativeZero (0 :: Float), isDenormalized (1 :: Float))
  print ("denormalized", isDenormalized (1.0e-45 :: Float), isDenormalized (1.17549435e-38 :: Float))
  print ("show", 3.4028235e38 :: Float, 1.0e-45 :: Float, 0.1 :: Float, 16777216 :: Float)
  print ("atan2", atan2 (0 :: Float) 1, isNegativeZero (atan2 (-0.0 :: Float) 1), abs (atan2 (1 :: Float) 1 - pi / 4) < 1.0e-5)
