-- Haskell 2010 Report 17 (Data.Complex): Floating instance (pi, exp, log, sqrt, (**), logBase, trigonometric
-- and hyperbolic functions and their inverses); exact cases printed, the rest checked against a tolerance
module Main (main) where

import Data.Complex

type C = Complex Double

closeC :: C -> C -> Bool
closeC a b = magnitude (a - b) < 1.0e-9

z :: C
z = 0.3 :+ 0.2

i :: C
i = 0 :+ 1

main :: IO ()
main = do
  print ("pi", realPart (pi :: C) == pi, imagPart (pi :: C) == 0)
  print ("exp zero", exp (0 :+ 0) :: C)
  print ("log one", log (1 :+ 0) :: C)
  print ("sqrt zero", sqrt (0 :+ 0) :: C)
  print ("sqrt positive real", sqrt (4 :+ 0) :: C)
  print ("sqrt negative real", sqrt ((-4) :+ 0) :: C)
  print ("sqrt imaginary", sqrt (0 :+ 2) :: C)
  print ("sqrt 3 4", sqrt (3 :+ 4) :: C)
  print ("sqrt 3 -4", sqrt (3 :+ (-4)) :: C)
  print ("sin zero", sin (0 :+ 0) == (0 :: C))
  print ("cos zero", cos (0 :+ 0) == (1 :: C))
  print ("sinh zero", sinh (0 :+ 0) == (0 :: C))
  print ("cosh zero", cosh (0 :+ 0) == (1 :: C))
  print ("euler identity", closeC (exp (i * pi) + 1) 0)
  print ("exp of sum", closeC (exp (z + i)) (exp z * exp i))
  print ("exp . log", closeC (exp (log z)) z)
  print ("log . exp", closeC (log (exp z)) z)
  print ("log of i", closeC (log i) (0 :+ (pi / 2)))
  print ("log of minus one", closeC (log ((-1) :+ 0)) (0 :+ pi))
  print ("sqrt squared", closeC (sqrt z * sqrt z) z)
  print ("sqrt of minus one", closeC (sqrt ((-1) :+ 0)) i)
  print ("power real", closeC ((2 :+ 0) ** (3 :+ 0)) 8)
  print ("power square", closeC (z ** 2) (z * z))
  print ("power half is sqrt", closeC (z ** 0.5) (sqrt z))
  print ("i to the i is real", closeC (i ** i) (exp (-pi / 2) :+ 0))
  print ("logBase", closeC (logBase 2 8) 3)
  print ("logBase definition", closeC (logBase z i) (log i / log z))
  print ("sin cos pythagoras", closeC (sin z * sin z + cos z * cos z) 1)
  print ("tan is sin over cos", closeC (tan z) (sin z / cos z))
  print ("sin of imaginary", closeC (sin (0 :+ 1)) (0 :+ sinh 1))
  print ("cos of imaginary", closeC (cos (0 :+ 1)) (cosh 1 :+ 0))
  print ("sin . asin", closeC (sin (asin z)) z)
  print ("asin . sin", closeC (asin (sin z)) z)
  print ("cos . acos", closeC (cos (acos z)) z)
  print ("tan . atan", closeC (tan (atan z)) z)
  print ("atan . tan", closeC (atan (tan z)) z)
  print ("cosh sinh identity", closeC (cosh z * cosh z - sinh z * sinh z) 1)
  print ("tanh is sinh over cosh", closeC (tanh z) (sinh z / cosh z))
  print ("sinh . asinh", closeC (sinh (asinh z)) z)
  print ("asinh . sinh", closeC (asinh (sinh z)) z)
  print ("cosh . acosh", closeC (cosh (acosh z)) z)
  print ("tanh . atanh", closeC (tanh (atanh z)) z)
  print ("atanh . tanh", closeC (atanh (tanh z)) z)
  print ("real agrees with Double", closeC (sin (0.5 :+ 0)) (sin 0.5 :+ 0), closeC (exp (2 :+ 0)) (exp 2 :+ 0))
