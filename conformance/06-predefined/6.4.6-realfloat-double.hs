-- Haskell 2010 Report 6.4.6: class RealFloat at Double (IEEE double): floatRadix, floatDigits, floatRange, decodeFloat,
-- encodeFloat, exponent, significand, scaleFloat
d :: Double -> Double
d = id

values :: [Double]
values = [1, -1, 0.5, 8, 0.1, -1.5, 3.0e10, 1.0e-10, 123456.789, 1.0e300, 1.0e-300]

main :: IO ()
main = do
  print ("floatRadix", floatRadix (d 1))
  print ("floatDigits", floatDigits (d 1))
  print ("floatRange", floatRange (d 1))
  print ("argument not evaluated", floatRadix (undefined :: Double), floatDigits (undefined :: Double), floatRange (undefined :: Double))
  print ("decodeFloat 1", decodeFloat (d 1))
  print ("decodeFloat 0", decodeFloat (d 0))
  print ("decodeFloat 0.5, 2, 8", decodeFloat (d 0.5), decodeFloat (d 2), decodeFloat (d 8))
  print ("decodeFloat -1.5", decodeFloat (d (-1.5)))
  print ("decodeFloat 0.1", decodeFloat (d 0.1))
  print ("decodeFloat 1.0e300", decodeFloat (d 1.0e300))
  print ("decodeFloat smallest normal", decodeFloat (encodeFloat 1 (-1022) :: Double))
  print ("mantissa range", and [let (m, _) = decodeFloat x in 2 ^ (52 :: Int) <= abs m && abs m < 2 ^ (53 :: Int) | x <- values])
  print ("encodeFloat", encodeFloat 1 10 :: Double, encodeFloat 3 (-1) :: Double, encodeFloat (-5) 2 :: Double, encodeFloat 0 100 :: Double)
  print ("encodeFloat large exponent", encodeFloat 1 1023 :: Double, encodeFloat 1 (-1022) :: Double)
  print ("encodeFloat largest finite", encodeFloat (2 ^ (53 :: Int) - 1) 971 :: Double)
  print ("encode . decode", and [uncurry encodeFloat (decodeFloat x) == x | x <- values])
  print ("exponent", exponent (d 1), exponent (d 8), exponent (d 0.5), exponent (d 0.1), exponent (d 0), exponent (d (-8)))
  print ("significand", significand (d 1), significand (d 8), significand (d 0.75), significand (d (-8)), significand (d 0))
  print ("significand range", and [let s = abs (significand x) in 0.5 <= s && s < 1 | x <- values])
  print ("significand * 2^exponent", and [significand x * 2 ^^ exponent x == x | x <- values])
  print ("scaleFloat", scaleFloat 3 (d 1.5), scaleFloat (-2) (d 1), scaleFloat 0 (d 0.1), scaleFloat 5 (d 0), scaleFloat 10 (d (-1)))
  print ("scaleFloat law", and [scaleFloat 7 x == x * 128 && scaleFloat (-7) x == x / 128 | x <- values])
  print ("epsilon", encodeFloat 1 (-52) :: Double, 1 + encodeFloat 1 (-52) > d 1, 1 + encodeFloat 1 (-53) == d 1)
  print ("exponent boundaries", exponent (encodeFloat 1 1023 :: Double), exponent (encodeFloat 1 (-1022) :: Double))
  print ("derived radix facts", floatRadix (d 1) ^ floatDigits (d 1), fst (floatRange (d 1)) + snd (floatRange (d 1)))
