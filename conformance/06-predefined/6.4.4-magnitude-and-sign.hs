-- Haskell 2010 Report 6.4.4: abs, signum, negate; abs x * signum x == x
import Data.Ratio ((%))

ints :: [Int]
ints = [-100, -5, -1, 0, 1, 5, 100, maxBound, minBound + 1]

integers :: [Integer]
integers = [negate (10 ^ (30 :: Int)), -5, 0, 5, 10 ^ (30 :: Int)]

doubles :: [Double]
doubles = [-1.0e300, -2.5, -0.5, 0.5, 2.5, 1.0e300]

rationals :: [Rational]
rationals = [(-7) % 2, (-1) % 3, 0, 1 % 3, 7 % 2]

main :: IO ()
main = do
  print ("abs Int", map abs [-5, 0, 5 :: Int], abs (minBound + 1 :: Int) == maxBound)
  print ("signum Int", map signum [-5, 0, 5 :: Int], signum (minBound :: Int), signum (maxBound :: Int))
  print ("negate Int", map negate [-5, 0, 5 :: Int], negate (maxBound :: Int) == minBound + 1)
  print ("abs Integer", map abs integers)
  print ("signum Integer", map signum integers)
  print ("negate Integer", map negate integers)
  print ("abs Double", map abs doubles)
  print ("signum Double", map signum doubles)
  print ("negate Double", map negate doubles)
  print ("abs Float", map abs [-1.5, 0.25 :: Float], map signum [-1.5, 0.25 :: Float])
  print ("abs Rational", map abs rationals)
  print ("signum Rational", map signum rationals)
  print ("negate Rational", map negate rationals)
  print ("zero Double", signum (0.0 :: Double), abs (0.0 :: Double), signum (0.0 :: Float))
  print ("negative zero compares as zero", signum (-0.0 :: Double) == 0, abs (-0.0 :: Double) == 0)
  print ("infinities", signum (1 / 0 :: Double), signum (-1 / 0 :: Double), abs (-1 / 0 :: Double))
  print ("law Int", and [abs x * signum x == x | x <- ints])
  print ("law Integer", and [abs x * signum x == x | x <- integers])
  print ("law Double", and [abs x * signum x == x | x <- doubles])
  print ("law Rational", and [abs x * signum x == x | x <- rationals])
  print ("abs is non-negative", all (>= 0) (map abs ints), all (>= 0) (map abs doubles), all (>= 0) (map abs rationals))
  print ("negate is an involution", map (negate . negate) ints == ints, map (negate . negate) rationals == rationals)
  print ("unary minus is negate", - (3 :: Int) == negate 3, (\x -> - x) (2.5 :: Double), - (-4) :: Int)
  print ("x - y == x + negate y", and [x - y == x + negate y | x <- ints, y <- ints])
  print ("subtract", subtract 1 (10 :: Int), map (subtract 2) [1, 2, 3 :: Int], subtract (1 % 2) (1 :: Rational))
