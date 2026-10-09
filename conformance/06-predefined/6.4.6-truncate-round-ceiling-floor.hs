-- Haskell 2010 Report 6.4.6: truncate, round (to nearest, ties to even), ceiling, floor
import Data.Ratio ((%))

row :: Double -> (Double, Integer, Integer, Integer, Integer)
row x = (x, truncate x, round x, ceiling x, floor x)

rowQ :: Rational -> (Rational, Integer, Integer, Integer, Integer)
rowQ x = (x, truncate x, round x, ceiling x, floor x)

rowF :: Float -> (Float, Int, Int, Int, Int)
rowF x = (x, truncate x, round x, ceiling x, floor x)

samples :: [Double]
samples = [-1.0e6, -7.5, -2.5, -2.4, -1.0, -0.5, -0.25, 0.0, 0.25, 0.5, 1.0, 2.4, 2.5, 7.5, 1.0e6, 123456.789]

main :: IO ()
main = do
  putStrLn "(x, truncate, round, ceiling, floor)"
  mapM_ (print . row) [2.5, 3.5, -2.5, -3.5, 0.5, 1.5, -0.5, -1.5]
  mapM_ (print . row) [2.4, 2.6, -2.4, -2.6, 0.49, 0.51, -0.49, -0.51]
  mapM_ (print . row) [0.0, 7.0, -7.0, 1.0e-10, -1.0e-10, 0.999999, -0.999999]
  mapM_ (print . row) [4.5, 5.5, 6.5, -4.5, -5.5, 1000000.5, 1000001.5]
  mapM_ (print . row) [1.0e15, 1.0e15 + 0.5, 1.0e15 + 1.5, 1.0e20, -1.0e20, 123456789.987]
  putStrLn "Rational"
  mapM_ (print . rowQ) [5 % 2, 7 % 2, (-5) % 2, (-7) % 2, 1 % 2, (-1) % 2, 3 % 2]
  mapM_ (print . rowQ) [1 % 3, 2 % 3, (-1) % 3, (-2) % 3, 4, -4, 0]
  mapM_ (print . rowQ) [(10 ^ (30 :: Int) + 1) % 2, negate (10 ^ (30 :: Int) + 1) % 2]
  putStrLn "Float to Int"
  mapM_ (print . rowF) [2.5, 3.5, -2.5, -3.5, 0.5, -0.5, 2.25, -2.75, 16777216]
  print ("result at Int", truncate (2.7 :: Double) :: Int, round (2.5 :: Double) :: Int, ceiling (2.1 :: Double) :: Int, floor (-2.1 :: Double) :: Int)
  print ("round ties over a range", map round [-4.5, -3.5 .. 4.5 :: Double] :: [Int])
  print ("round halves of rationals", map (round . (% 2)) [-7, -5 .. 7 :: Integer] :: [Integer])
  print ("floor <= truncate <= ceiling", and [floor x <= (truncate x :: Integer) && truncate x <= (ceiling x :: Integer) | x <- samples])
  print ("floor <= x <= ceiling", and [fromIntegral (floor x :: Integer) <= x && x <= fromIntegral (ceiling x :: Integer) | x <- samples])
  print ("ceiling - floor", and [(ceiling x :: Integer) - floor x == (if fromIntegral (floor x :: Integer) == x then 0 else 1) | x <- samples])
  print ("truncate is toward zero", and [(truncate x :: Integer) == (if x < 0 then ceiling x else floor x) | x <- samples])
  print ("round is within a half", and [abs (fromIntegral (round x :: Integer) - x) <= 0.5 | x <- samples])
