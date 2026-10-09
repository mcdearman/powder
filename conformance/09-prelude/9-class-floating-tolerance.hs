-- Haskell 2010 Report 9: class Floating, all methods checked against a tolerance (only Bools are printed)
module Main (main) where

near :: Double -> Double -> Bool
near x y = abs (x - y) < 1.0e-9

nearF :: Float -> Float -> Bool
nearF x y = abs (x - y) < 1.0e-4

e :: Double
e = exp 1

main :: IO ()
main = do
  print ("pi", near pi 3.141592653589793, nearF pi 3.1415927)
  print ("exp", near e 2.718281828459045, near (exp 2) (e * e), near (exp (-1)) (1 / e))
  print ("log", near (log e) 1, near (log (e * e)) 2, near (log 10) 2.302585092994046)
  print ("exp . log", all (\x -> near (exp (log x)) x) [0.5, 1, 2, 10])
  print ("sqrt", near (sqrt 2 * sqrt 2) 2, near (sqrt 10) 3.1622776601683795)
  print ("**", near (2 ** 0.5) (sqrt 2), near (10 ** (-2)) 0.01, near (e ** 2) (exp 2), near (9 ** 1.5) 27)
  print ("logBase", near (logBase 2 8) 3, near (logBase 10 1000) 3, near (logBase 2 0.5) (-1), near (logBase e 5) (log 5))
  print ("sin", near (sin (pi / 2)) 1, near (sin pi) 0, near (sin (pi / 6)) 0.5, near (sin (-pi / 2)) (-1))
  print ("cos", near (cos pi) (-1), near (cos (pi / 2)) 0, near (cos (pi / 3)) 0.5)
  print ("tan", near (tan (pi / 4)) 1, near (tan 1) (sin 1 / cos 1))
  print ("asin", near (asin 1) (pi / 2), near (asin 0.5) (pi / 6), near (asin (-1)) (-pi / 2))
  print ("acos", near (acos 0) (pi / 2), near (acos (-1)) pi, near (acos 0.5) (pi / 3))
  print ("atan", near (atan 1) (pi / 4), near (atan (-1)) (-pi / 4), near (atan 1.0e12) (pi / 2))
  print ("sinh", near (sinh 1) ((e - 1 / e) / 2), near (sinh (-1)) (negate (sinh 1)))
  print ("cosh", near (cosh 1) ((e + 1 / e) / 2), near (cosh (-1)) (cosh 1))
  print ("tanh", near (tanh 1) (sinh 1 / cosh 1), near (tanh 20) 1)
  print ("asinh", near (asinh (sinh 1)) 1, near (asinh 0) 0, near (asinh (sinh (-2))) (-2))
  print ("acosh", near (acosh (cosh 1)) 1, near (acosh 1) 0, near (acosh (cosh 3)) 3)
  print ("atanh", near (atanh (tanh 1)) 1, near (atanh 0) 0, near (atanh (-0.5)) (negate (atanh 0.5)))
  print ("identity", all (\x -> near (sin x * sin x + cos x * cos x) 1) [0, 0.5, 1, 2, 3, -4])
  print ("Float", nearF (exp 1) 2.7182817, nearF (sin (pi / 2)) 1, nearF (sqrt 2 * sqrt 2) 2, nearF (logBase 2 8) 3)
  print ("Float more", nearF (atan 1) (pi / 4), nearF (cosh 0) 1, nearF (2 ** 0.5) (sqrt 2), nearF (log (exp 2)) 2)
