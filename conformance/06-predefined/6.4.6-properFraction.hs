-- Haskell 2010 Report 6.4.6: properFraction x = (n, f) with n integral, |f| < 1, n + f == x, f having the sign of x
import Data.Ratio ((%))

pf :: Double -> (Integer, Double)
pf = properFraction

pfQ :: Rational -> (Integer, Rational)
pfQ = properFraction

doubles :: [Double]
doubles = [3.75, -3.75, 0.5, -0.5, 4.0, 0.0, 1.0e10 + 0.25, -1.0e10 - 0.25, 123.125, -0.001953125]

rationals :: [Rational]
rationals = [7 % 2, (-7) % 2, 1 % 3, (-1) % 3, 5, -5, 0, 22 % 7, (-22) % 7]

main :: IO ()
main = do
  print ("positive", pf 3.75, pf 0.5, pf 123.125)
  print ("negative", pf (-3.75), pf (-0.5), pf (-123.125))
  print ("integral values", pf 4.0, pf 0.0, pf 1.0e15)
  print ("negative integral value: fraction equals zero", fst (pf (-4.0)), snd (pf (-4.0)) == 0)
  print ("large", pf 1.0e20, pf (1.0e10 + 0.25))
  print ("small", pf 1.0e-5, pf (-0.001953125))
  print ("result at Int", properFraction (2.5 :: Double) :: (Int, Double), properFraction (-2.5 :: Double) :: (Int, Double))
  print ("Float", properFraction (2.75 :: Float) :: (Int, Float), properFraction (-2.75 :: Float) :: (Integer, Float))
  print ("Rational positive", pfQ (7 % 2), pfQ (1 % 3), pfQ (22 % 7))
  print ("Rational negative", pfQ ((-7) % 2), pfQ ((-1) % 3), pfQ ((-22) % 7))
  print ("Rational integral", pfQ 5, pfQ (-5), pfQ 0)
  print ("Rational big", pfQ ((10 ^ (30 :: Int) + 1) % 3))
  print ("n + f == x Double", and [fromIntegral (fst (pf x)) + snd (pf x) == x | x <- doubles])
  print ("n + f == x Rational", and [fromIntegral (fst (pfQ x)) + snd (pfQ x) == x | x <- rationals])
  print ("|f| < 1", and [abs (snd (pf x)) < 1 | x <- doubles], and [abs (snd (pfQ x)) < 1 | x <- rationals])
  print ("f has the sign of x or is zero", and [snd (pf x) == 0 || signum (snd (pf x)) == signum x | x <- doubles])
  print ("n is truncate x", and [fst (pf x) == truncate x | x <- doubles], and [fst (pfQ x) == truncate x | x <- rationals])
  print ("pattern use", let (n, f) = pf 9.5 in (n * 2, f * 2))
