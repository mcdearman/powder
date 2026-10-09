-- Haskell 2010 Report 17.1 (Data.Complex): Complex((:+)), realPart, imagPart, conjugate; (:+) is infix 6
module Main (main) where

import Data.Complex

z :: Complex Double
z = 3 :+ 4

w :: Complex Float
w = 1.5 :+ (-2.5)

swapParts :: Complex Double -> Complex Double
swapParts (a :+ b) = b :+ a

main :: IO ()
main = do
  print ("realPart", realPart z)
  print ("imagPart", imagPart z)
  print ("float parts", realPart w, imagPart w)
  print ("pattern match", swapParts z)
  print ("constructor as function", zipWith (:+) [1, 2] [3, 4 :: Double])
  print ("section", map (:+ 0) [1, 2 :: Double])
  print ("conjugate", conjugate z)
  print ("conjugate negative imaginary", conjugate (1 :+ (-2) :: Complex Double))
  print ("conjugate twice", conjugate (conjugate z) == z)
  print ("conjugate float", conjugate w)
  print ("rebuild from parts", (realPart z :+ imagPart z) == z)
  print ("z times conjugate", z * conjugate z)
  print ("z plus conjugate", z + conjugate z)
  -- infix 6 :+ is the same level as + and -, tighter than ==, looser than *
  print ("times binds tighter", 2 * 3 :+ 4 * 5 :: Complex Double)
  print ("comparison binds looser", 1 :+ 2 == (1 :+ 2 :: Complex Double))
  print ("literal is real", (5 :: Complex Double))
  print ("fractional literal is real", (2.5 :: Complex Double))
  print ("case", case z of { 0 :+ _ -> "imaginary"; _ :+ 0 -> "real"; _ -> "general" })
  print ("case real", case (7 :+ 0 :: Complex Double) of { 0 :+ _ -> "imaginary"; _ :+ 0 -> "real"; _ -> "general" })
