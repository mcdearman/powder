-- Haskell 2010 Report 9: class RealFloat at Double (floatRadix floatDigits floatRange decodeFloat encodeFloat exponent significand scaleFloat)
module Main (main) where

samples :: [Double]
samples = [1, 0.5, 8, -1.5, 0.1, 123456.789, 1.0e300, 1.0e-300, -0.75, 3]

roundTrip :: Double -> Bool
roundTrip x = uncurry encodeFloat (decodeFloat x) == x

mantissaInRange :: Double -> Bool
mantissaInRange x = lo <= abs m && abs m < 2 * lo
  where
    (m, _) = decodeFloat x
    lo = 2 ^ (52 :: Int)

sigExpLaw :: Double -> Bool
sigExpLaw x = significand x * 2 ^^ exponent x == x && abs (significand x) >= 0.5 && abs (significand x) < 1

main :: IO ()
main = do
  print ("floatRadix", floatRadix (1 :: Double))
  print ("floatDigits", floatDigits (1 :: Double))
  print ("floatRange", floatRange (1 :: Double))
  print ("floatRadix lazy in argument", floatRadix (undefined :: Double), floatDigits (undefined :: Double))
  print ("decodeFloat 1", decodeFloat (1 :: Double))
  print ("decodeFloat 0.5", decodeFloat (0.5 :: Double))
  print ("decodeFloat 0", decodeFloat (0 :: Double))
  print ("decodeFloat -1.5", decodeFloat (-1.5 :: Double))
  print ("decodeFloat 0.1", decodeFloat (0.1 :: Double))
  print ("decodeFloat max", decodeFloat (1.7976931348623157e308 :: Double))
  print ("decodeFloat min normal", decodeFloat (2.2250738585072014e-308 :: Double))
  print ("decodeFloat denormal", decodeFloat (5.0e-324 :: Double))
  print ("encodeFloat", encodeFloat 1 10 :: Double, encodeFloat 3 (-1) :: Double, encodeFloat 0 100 :: Double, encodeFloat (-5) 2 :: Double)
  print ("encodeFloat big mantissa", encodeFloat (2 ^ (60 :: Int)) (-60) :: Double, encodeFloat 4503599627370496 (-52) :: Double)
  print ("round trip", all roundTrip samples, roundTrip 0, roundTrip 5.0e-324)
  print ("mantissa range", all mantissaInRange samples)
  print ("exponent", map exponent [8, 1, 0.5, 0.1, 0, -8, 1.0e300 :: Double])
  print ("significand", map significand [8, 1, 0.5, 0.75, 0, -8, 3 :: Double])
  print ("significand/exponent law", all sigExpLaw samples)
  print ("scaleFloat", scaleFloat 3 (1.5 :: Double), scaleFloat (-1) (1 :: Double), scaleFloat 5 (0 :: Double), scaleFloat 0 (2.5 :: Double))
  print ("scaleFloat negative", scaleFloat 10 (-1 :: Double), scaleFloat (-10) (1024 :: Double))
  print ("exponent = snd decode + digits", all (\x -> exponent x == snd (decodeFloat x) + floatDigits x) samples)
