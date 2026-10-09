-- Haskell 2010 Report 6.4 (and chapter 20, Data.Ratio): Rational = Ratio Integer; (%), numerator, denominator,
-- normalisation (lowest terms, positive denominator), arithmetic, Show at precedence 7, Read
import Data.Ratio (Ratio, denominator, numerator, (%))

r :: Rational -> Rational
r = id

parts :: Rational -> (Integer, Integer)
parts x = (numerator x, denominator x)

main :: IO ()
main = do
  print (r (1 % 2))
  print ("reduced to lowest terms", r (6 % 8), r (100 % 10), r (0 % 5), r (17 % 17))
  print ("sign lives in the numerator", r (3 % (-4)), r ((-3) % 4), r ((-3) % (-4)), r (0 % (-7)))
  print ("numerator/denominator", parts (6 % 8), parts (3 % (-4)), parts 5, parts 0, parts (-2))
  print ("arithmetic", r (1 % 2 + 1 % 3), r (1 % 2 - 1 % 3), r ((1 % 2) * (2 % 3)), r ((1 % 2) / (3 % 4)))
  print ("arithmetic cancels", r (1 % 6 + 1 % 3), r ((2 % 3) * (3 % 2)), r (1 % 2 - 1 % 2))
  print ("negate/abs/signum", negate (r (1 % 2)), abs (r ((-1) % 2)), signum (r ((-1) % 2)), signum (r 0))
  print ("recip", recip (r (3 % 4)), recip (r ((-3) % 4)), recip (r 5))
  print ("literals", r 3, r 0.75, r (-1.5), r 1.0e-2)
  print ("comparison", r (1 % 3) < 1 % 2, r (2 % 4) == 1 % 2, compare (r ((-1) % 2)) ((-1) % 3), max (r (2 % 3)) (3 % 5))
  print ("fromIntegral/fromRational", fromIntegral (7 :: Int) :: Rational, fromRational (r (7 % 8)) :: Double)
  print ("toRational", toRational (0.375 :: Double), toRational (4 :: Int))
  print ("rounding", truncate (r ((-7) % 2)) :: Integer, round (r (5 % 2)) :: Integer, floor (r ((-7) % 2)) :: Integer, ceiling (r (7 % 2)) :: Integer)
  print ("powers", r (2 % 3) ^ (3 :: Int), r (2 % 3) ^^ (-3 :: Int), r ((-1) % 2) ^ (3 :: Int))
  putStrLn ("show: " ++ show (r (1 % 2)) ++ " | " ++ show (r ((-1) % 2)) ++ " | " ++ show (r 3))
  putStrLn ("showsPrec 7: " ++ showsPrec 7 (r (1 % 2)) "")
  putStrLn ("showsPrec 8: " ++ showsPrec 8 (r (1 % 2)) "")
  print (Just (r (1 % 2)))
  print [r (1 % 2), (-3) % 4]
  print (r (1 % 2), Left (r (5 % 3)) :: Either Rational Int)
  print ("read", read "1 % 2" :: Rational, read "(-3) % 4" :: Rational, read " ( 5 % 1 ) " :: Rational)
  print ("read . show", (read (show (r ((-22) % 7))) :: Rational) == (-22) % 7)
  print ("Ratio Int", (6 % 8 :: Ratio Int), numerator (6 % 8 :: Ratio Int), denominator (6 % (-8) :: Ratio Int))
  print ("% is infixl 7", r (1 % 2 * 4), r (2 * 1 % 4), r (1 + 1 % 2))
  print ("big", r ((10 ^ (30 :: Int)) % (10 ^ (28 :: Int) * 4)), r ((2 ^ (100 :: Int) + 1) % (2 ^ (100 :: Int))) - 1)
  print ("exact decimal arithmetic", r 0.1 + r 0.2 == r 0.3, sum (replicate 10 (r 0.1)) == 1)
