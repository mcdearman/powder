-- Haskell 2010 Report 6.4.6: fromIntegral, realToFrac, toInteger, fromInteger, toRational, fromRational
import Data.Ratio ((%))

average :: [Int] -> Double
average ns = fromIntegral (sum ns) / fromIntegral (length ns)

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / fromIntegral (length xs)

main :: IO ()
main = do
  print ("Int -> Integer", fromIntegral (42 :: Int) :: Integer, toInteger (minBound :: Int), fromIntegral (maxBound :: Int) + 1 :: Integer)
  print ("Integer -> Int", fromIntegral (42 :: Integer) :: Int, fromInteger (-7) :: Int)
  print ("Int -> Double", fromIntegral (3 :: Int) :: Double, fromIntegral (-3 :: Int) :: Double, fromIntegral (2 ^ (53 :: Int) :: Int) :: Double)
  print ("Int -> Double rounds to even", fromIntegral (2 ^ (53 :: Int) + 1 :: Int) :: Double, fromIntegral (2 ^ (53 :: Int) + 3 :: Int) :: Double)
  print ("Int -> Float", fromIntegral (3 :: Int) :: Float, fromIntegral (16777216 :: Int) :: Float, fromIntegral (16777217 :: Int) :: Float)
  print ("Integer -> Double", fromIntegral (10 ^ (15 :: Int) :: Integer) :: Double, fromInteger (2 ^ (70 :: Int)) :: Double)
  print ("Integer -> Float", fromInteger 12345 :: Float, fromInteger (2 ^ (30 :: Int)) :: Float)
  print ("Int -> Rational", fromIntegral (5 :: Int) :: Rational, toRational (-5 :: Int))
  print ("toRational Double", toRational (0.75 :: Double), toRational (-2.5 :: Double), toRational (3 :: Double))
  print ("toRational 0.1 is the exact binary value", toRational (0.1 :: Double))
  print ("toRational Float", toRational (0.5 :: Float), toRational (0.1 :: Float))
  print ("toRational Integer and Rational", toRational (2 ^ (70 :: Int) :: Integer), toRational (3 % 4 :: Rational))
  print ("fromRational", fromRational (3 % 4) :: Double, fromRational ((-5) % 2) :: Double, fromRational (1 % 3) :: Double)
  print ("fromRational Float", fromRational (3 % 4) :: Float, fromRational (1 % 3) :: Float)
  print ("fromRational . toRational", fromRational (toRational (0.1 :: Double)) == (0.1 :: Double), fromRational (toRational (1.0e-300 :: Double)) == (1.0e-300 :: Double))
  print ("realToFrac Float -> Double", realToFrac (0.5 :: Float) :: Double, realToFrac (0.1 :: Float) :: Double)
  print ("realToFrac Double -> Float", realToFrac (0.5 :: Double) :: Float, realToFrac (0.1 :: Double) :: Float, realToFrac (1.0e-3 :: Double) :: Float)
  print ("realToFrac Int -> Double", realToFrac (7 :: Int) :: Double, realToFrac (-7 :: Integer) :: Float)
  print ("realToFrac Rational -> Double", realToFrac (7 % 8 :: Rational) :: Double, realToFrac (1.25 :: Double) :: Rational)
  print ("realToFrac identity", realToFrac (2.5 :: Double) :: Double)
  print ("average", average [1, 2, 3, 4], average [1, 2])
  print ("mean overloaded", mean [1, 2, 3 :: Int] :: Double, mean [1.5, 2.5 :: Double] :: Double, mean [1, 2 :: Integer] :: Rational)
  print ("fromIntegral in mixed arithmetic", fromIntegral (length "abc") * 1.5 :: Double, sqrt (fromIntegral (16 :: Int)) :: Double)
  print ("truncate back", truncate (fromIntegral (123456789 :: Int) :: Double) :: Int, round (fromIntegral (maxBound :: Int) / 2 :: Double) == (2 ^ (62 :: Int) :: Integer))
