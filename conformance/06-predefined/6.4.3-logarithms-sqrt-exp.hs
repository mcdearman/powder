-- Haskell 2010 Report 6.4.3: sqrt, exp, log, logBase; exact cases printed, the rest compared within a tolerance
close :: Double -> Double -> Bool
close a b = abs (a - b) <= 1.0e-9 * max 1 (abs b)

main :: IO ()
main = do
  print ("sqrt exact", sqrt 16 :: Double, sqrt 0 :: Double, sqrt 1 :: Double, sqrt 2.25 :: Double, sqrt 0.25 :: Double)
  print ("sqrt exact large", sqrt 1048576 :: Double, sqrt 1.0e100 == (1.0e50 :: Double))
  print ("sqrt at Float", sqrt 9 :: Float, sqrt 0.25 :: Float)
  print ("sqrt close", close (sqrt 2 * sqrt 2) 2, close (sqrt 2) 1.4142135623730951, close (sqrt 1.0e-10) 1.0e-5)
  print ("sqrt negative is NaN", isNaN (sqrt (-1) :: Double), isNaN (sqrt (-0.5) :: Float))
  print ("exp/log exact", exp 0 :: Double, log 1 :: Double, exp 0 :: Float, log 1 :: Float)
  print ("exp close", close (exp 1) 2.718281828459045, close (exp 2) (exp 1 * exp 1), close (exp (-1)) (1 / exp 1))
  print ("log close", close (log (exp 2)) 2, close (log 10) 2.302585092994046, close (log 0.5) (negate (log 2)))
  print ("log of product", close (log 6) (log 2 + log 3), close (exp (log 7)) 7)
  print ("logBase close", close (logBase 2 8) 3, close (logBase 10 1000) 3, close (logBase 2 0.5) (-1), close (logBase 3 81) 4)
  print ("logBase definition", close (logBase 7 50) (log 50 / log 7), close (logBase 2 2) 1, close (logBase 5 1) 0)
  print ("log 0 is -Infinity", isInfinite (log 0 :: Double), (log 0 :: Double) < 0)
  print ("log negative is NaN", isNaN (log (-1) :: Double))
  print ("exp overflow/underflow", isInfinite (exp 1000 :: Double), (exp (-1000) :: Double) == 0)
  print ("sqrt of infinity", isInfinite (sqrt (1 / 0) :: Double))
  print ("sqrt as power", close (sqrt 10) (10 ** 0.5), close (sqrt 3 ^ (2 :: Int)) 3)
  print ("floor of sqrt", floor (sqrt (99 :: Double)) :: Int, floor (sqrt (100 :: Double)) :: Int, ceiling (sqrt (101 :: Double)) :: Int)
  print ("Float tolerance", abs (exp 1 - 2.7182817 :: Float) < 1.0e-5, abs (logBase 2 1024 - 10 :: Float) < 1.0e-4)
