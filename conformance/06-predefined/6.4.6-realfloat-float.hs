-- Haskell 2010 Report 6.4.6: class RealFloat at Float (IEEE single): floatRadix, floatDigits, floatRange, decodeFloat,
-- encodeFloat, exponent, significand, scaleFloat, and the classification predicates
f :: Float -> Float
f = id

values :: [Float]
values = [1, -1, 0.5, 8, 0.1, -1.5, 3.0e10, 1.0e-10, 123456.79, 1.0e30, 1.0e-30]

main :: IO ()
main = do
  print ("floatRadix", floatRadix (f 1))
  print ("floatDigits", floatDigits (f 1))
  print ("floatRange", floatRange (f 1))
  print ("decodeFloat 1", decodeFloat (f 1))
  print ("decodeFloat 0", decodeFloat (f 0))
  print ("decodeFloat 0.5, 2, 8", decodeFloat (f 0.5), decodeFloat (f 2), decodeFloat (f 8))
  print ("decodeFloat -1.5", decodeFloat (f (-1.5)))
  print ("decodeFloat 0.1", decodeFloat (f 0.1))
  print ("mantissa range", and [let (m, _) = decodeFloat x in 2 ^ (23 :: Int) <= abs m && abs m < 2 ^ (24 :: Int) | x <- values])
  print ("encodeFloat", encodeFloat 1 10 :: Float, encodeFloat 3 (-1) :: Float, encodeFloat (-5) 2 :: Float, encodeFloat 0 50 :: Float)
  print ("encodeFloat largest finite", encodeFloat (2 ^ (24 :: Int) - 1) 104 :: Float)
  print ("encodeFloat smallest normal", encodeFloat 1 (-126) :: Float)
  print ("encode . decode", and [uncurry encodeFloat (decodeFloat x) == x | x <- values])
  print ("exponent", exponent (f 1), exponent (f 8), exponent (f 0.5), exponent (f 0), exponent (f (-8)))
  print ("significand", significand (f 1), significand (f 8), significand (f 0.75), significand (f (-8)), significand (f 0))
  print ("significand * 2^exponent", and [significand x * 2 ^^ exponent x == x | x <- values])
  print ("scaleFloat", scaleFloat 3 (f 1.5), scaleFloat (-2) (f 1), scaleFloat 5 (f 0))
  print ("epsilon", encodeFloat 1 (-23) :: Float, 1 + encodeFloat 1 (-23) > f 1, 1 + encodeFloat 1 (-24) == f 1)
  print ("isNaN", isNaN (f 0 / 0), isNaN (f 1), isNaN (f 1 / 0))
  print ("isInfinite", isInfinite (f 1 / 0), isInfinite (f (-1) / 0), isInfinite (f 1), isInfinite (f 0 / 0))
  print ("isInfinite by overflow", isInfinite (f 3.0e38 * 10), isInfinite (f 3.0e38))
  print ("isDenormalized", isDenormalized (encodeFloat 1 (-149) :: Float), isDenormalized (encodeFloat 1 (-126) :: Float), isDenormalized (f 0), isDenormalized (f 1))
  print ("isNegativeZero", isNegativeZero (f (-0.0)), isNegativeZero (f 0), isNegativeZero (f (-1)), isNegativeZero (negate (f 0)))
  print ("isIEEE", isIEEE (f 1))
  print ("single precision is visible", f 16777216 + 1 == f 16777216, f 0.1 + f 0.2 == f 0.3, realToFrac (f 0.1) == (0.1 :: Double))
