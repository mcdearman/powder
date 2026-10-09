-- Haskell 2010 Report 17.2 (Data.Complex): mkPolar, cis, polar, magnitude, phase
-- (exact results printed; inexact ones compared against a tolerance)
module Main (main) where

import Data.Complex

type C = Complex Double

close :: Double -> Double -> Bool
close a b = abs (a - b) < 1.0e-9

closeC :: C -> C -> Bool
closeC a b = magnitude (a - b) < 1.0e-9

main :: IO ()
main = do
  print ("magnitude 3 4", magnitude (3 :+ 4 :: C))
  print ("magnitude negative parts", magnitude ((-3) :+ (-4) :: C))
  print ("magnitude 5 12", magnitude (5 :+ 12 :: C))
  print ("magnitude zero", magnitude (0 :+ 0 :: C))
  print ("magnitude real", magnitude (1 :+ 0 :: C), magnitude ((-2) :+ 0 :: C))
  print ("magnitude imaginary", magnitude (0 :+ (-2) :: C))
  print ("magnitude float", magnitude (3 :+ 4 :: Complex Float))
  print ("magnitude 1 1", close (magnitude (1 :+ 1 :: C)) (sqrt 2))
  print ("magnitude avoids overflow", close (magnitude (3.0e200 :+ 4.0e200 :: C) / 1.0e200) 5)
  print ("phase zero", phase (0 :+ 0 :: C))
  print ("phase positive real", phase (1 :+ 0 :: C))
  print ("phase positive imaginary", close (phase (0 :+ 1 :: C)) (pi / 2))
  print ("phase negative real", close (phase ((-1) :+ 0 :: C)) pi)
  print ("phase negative imaginary", close (phase (0 :+ (-1) :: C)) (-pi / 2))
  print ("phase first quadrant", close (phase (1 :+ 1 :: C)) (pi / 4))
  print ("phase second quadrant", close (phase ((-1) :+ 1 :: C)) (3 * pi / 4))
  print ("phase third quadrant", close (phase ((-1) :+ (-1) :: C)) (-3 * pi / 4))
  print ("phase fourth quadrant", close (phase (1 :+ (-1) :: C)) (-pi / 4))
  print ("phase range", all (\z -> phase z > negate pi - 1.0e-9 && phase z <= pi) [1 :+ 1, (-1) :+ 0, 0 :+ (-3), (-2) :+ (-2 :: Double)])
  print ("mkPolar angle zero", mkPolar 2 0 :: C)
  print ("mkPolar zero magnitude", mkPolar 0 0 :: C)
  print ("mkPolar quarter turn", closeC (mkPolar 2 (pi / 2)) (0 :+ 2))
  print ("mkPolar half turn", closeC (mkPolar 3 pi) ((-3) :+ 0))
  print ("cis zero", cis 0 :: C)
  print ("cis quarter turn", closeC (cis (pi / 2)) (0 :+ 1))
  print ("cis has magnitude one", close (magnitude (cis 1.234 :: C)) 1)
  print ("cis has the given phase", close (phase (cis 1.234 :: C)) 1.234)
  print ("cis is mkPolar 1", closeC (cis 0.7) (mkPolar 1 0.7))
  print ("polar zero", polar (0 :+ 0 :: C))
  print ("polar positive real", polar (2 :+ 0 :: C))
  print ("polar magnitude", fst (polar (3 :+ 4 :: C)))
  print ("polar phase", close (snd (polar (3 :+ 4 :: C))) (atan2 4 3))
  print ("polar is magnitude and phase", polar (3 :+ 4 :: C) == (magnitude (3 :+ 4 :: C), phase (3 :+ 4 :: C)))
  print ("mkPolar after polar", closeC (uncurry mkPolar (polar ((-2) :+ 5))) ((-2) :+ 5))
