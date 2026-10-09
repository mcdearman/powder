-- Haskell 2010 Report 6.4.5 (and atan2 from 6.4.6): trigonometric and hyperbolic functions; exact cases printed, others within tolerance
close :: Double -> Double -> Bool
close a b = abs (a - b) <= 1.0e-9 * max 1 (abs b)

angles :: [Double]
angles = [-3, -1.5, -0.25, 0, 0.5, 1, 2, 10]

main :: IO ()
main = do
  print ("pi", pi :: Double, pi :: Float)
  print ("exact at zero", sin 0 :: Double, cos 0 :: Double, tan 0 :: Double)
  print ("exact inverse", asin 0 :: Double, acos 1 :: Double, atan 0 :: Double)
  print ("exact hyperbolic", sinh 0 :: Double, cosh 0 :: Double, tanh 0 :: Double)
  print ("inverse hyperbolic at zero", close (asinh 0) 0, close (acosh 1) 0, close (atanh 0) 0)
  print ("exact at zero Float", sin 0 :: Float, cos 0 :: Float, atan 0 :: Float, cosh 0 :: Float)
  print ("sin", close (sin (pi / 2)) 1, close (sin pi) 0, close (sin (pi / 6)) 0.5, close (sin (-pi / 2)) (-1))
  print ("cos", close (cos pi) (-1), close (cos (pi / 2)) 0, close (cos (pi / 3)) 0.5, close (cos (2 * pi)) 1)
  print ("tan", close (tan (pi / 4)) 1, close (tan (-pi / 4)) (-1), close (tan 1) (sin 1 / cos 1))
  print ("asin/acos", close (asin 1) (pi / 2), close (asin (-1)) (-pi / 2), close (acos 0) (pi / 2), close (acos (-1)) pi)
  print ("atan", close (atan 1) (pi / 4), close (atan (-1)) (-pi / 4), close (atan 1.0e300) (pi / 2))
  print ("sin^2 + cos^2", and [close (sin x * sin x + cos x * cos x) 1 | x <- angles])
  print ("inverse round trips", and [close (sin (asin x)) x && close (cos (acos x)) x | x <- [-1, -0.5, 0, 0.3, 1]])
  print ("atan . tan", and [close (atan (tan x)) x | x <- [-1.5, -0.25, 0, 0.5, 1]])
  print ("odd and even", and [close (sin (-x)) (negate (sin x)) && close (cos (-x)) (cos x) | x <- angles])
  print ("hyperbolic identity", and [close (cosh x * cosh x - sinh x * sinh x) 1 | x <- [-3, -1.5, -0.25, 0, 0.5, 1, 2]])
  print ("hyperbolic via exp", close (sinh 1) ((exp 1 - exp (-1)) / 2), close (cosh 1) ((exp 1 + exp (-1)) / 2), close (tanh 1) (sinh 1 / cosh 1))
  print ("inverse hyperbolic", close (asinh (sinh 1)) 1, close (acosh (cosh 2)) 2, close (atanh (tanh 0.5)) 0.5)
  print ("tanh saturates", close (tanh 50) 1, close (tanh (-50)) (-1))
  print ("out of domain is NaN", isNaN (asin 2 :: Double), isNaN (acos (-2) :: Double))
  print ("atan2 exact", atan2 0 1 :: Double, atan2 0 0 == (0 :: Double))
  print ("atan2 axes", close (atan2 1 0) (pi / 2), close (atan2 0 (-1)) pi, close (atan2 (-1) 0) (-pi / 2))
  print ("atan2 quadrants", close (atan2 1 1) (pi / 4), close (atan2 1 (-1)) (3 * pi / 4), close (atan2 (-1) (-1)) (-3 * pi / 4), close (atan2 (-1) 1) (-pi / 4))
  print ("atan2 negative zero", close (atan2 (-0.0) (-1)) (-pi), close (atan2 0.0 (-1)) pi)
  print ("atan2 scale invariant", close (atan2 3 4) (atan2 6 8), close (atan2 3 4) (atan (3 / 4)))
  print ("atan2 at Float", abs (atan2 1 1 - pi / 4 :: Float) < 1.0e-6)
  print ("degrees", close (sin (30 * pi / 180)) 0.5, close (cos (60 * pi / 180)) 0.5)
