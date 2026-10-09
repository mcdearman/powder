-- Haskell 2010 Report 6.4.6: isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE at Double, and IEEE special-value arithmetic
nan :: Double
nan = 0 / 0

inf :: Double
inf = 1 / 0

negZero :: Double
negZero = -0.0

tiny :: Double
tiny = encodeFloat 1 (-1074)

smallestNormal :: Double
smallestNormal = encodeFloat 1 (-1022)

specials :: [Double]
specials = [nan, inf, negate inf, 0, negZero, 1, -1, tiny, smallestNormal, 1.0e308]

main :: IO ()
main = do
  print ("isIEEE", isIEEE (1 :: Double))
  print ("isNaN", map isNaN specials)
  print ("isInfinite", map isInfinite specials)
  print ("isDenormalized", map isDenormalized specials)
  print ("isNegativeZero", map isNegativeZero specials)
  print ("NaN sources", isNaN (inf - inf), isNaN (inf * 0), isNaN (inf / inf), isNaN (nan + 1), isNaN (sqrt (-1) :: Double))
  print ("NaN is not infinite", isInfinite nan, isNaN inf)
  print ("infinity sources", isInfinite (1.0e308 * 10 :: Double), isInfinite (negate 1.0e308 * 10 :: Double), isInfinite (1 / negZero))
  print ("infinity sign", inf > 0, negate inf < 0, 1 / negZero < 0, inf + inf == inf, negate inf == (-1) / 0)
  print ("infinity arithmetic", inf + 1 == inf, inf * (-1) == negate inf, 1 / inf == 0, isNegativeZero (1 / negate inf))
  print ("negative zero sources", isNegativeZero (negate 0 :: Double), isNegativeZero (0 * (-1) :: Double), isNegativeZero ((-1) / inf))
  print ("negative zero is not produced by", isNegativeZero (0 - 0 :: Double), isNegativeZero (negZero + 0), isNegativeZero (negZero * negZero))
  print ("negative zero equals zero", negZero == 0, negZero < 0, negZero >= 0)
  print ("denormals", tiny > 0, tiny / 2 == 0, isDenormalized (smallestNormal / 2), isDenormalized (tiny * 4))
  print ("denormal boundary", isDenormalized smallestNormal, isDenormalized (smallestNormal - tiny), smallestNormal - tiny < smallestNormal)
  print ("smallest denormal decoded (normalised mantissa)", decodeFloat tiny, tiny == 5.0e-324)
  print ("at most one classification", and [length (filter id [isNaN x, isInfinite x, isDenormalized x, isNegativeZero x]) <= 1 | x <- specials])
  print ("predicates on ordinary numbers", or (concat [[isNaN x, isInfinite x, isDenormalized x, isNegativeZero x] | x <- [1, -1, 0.1, 1.0e300, 1.0e-300]]))
