-- Haskell 2010 Report 6.4.1: numeric literals are overloaded; integer literals via fromInteger, fractional via fromRational
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Num Mod7 where
  Mod7 a + Mod7 b = Mod7 ((a + b) `mod` 7)
  Mod7 a * Mod7 b = Mod7 ((a * b) `mod` 7)
  negate (Mod7 a) = Mod7 (negate a `mod` 7)
  abs m = m
  signum (Mod7 a) = Mod7 (signum a)
  fromInteger n = Mod7 (n `mod` 7)

newtype Q = Q Rational deriving (Eq, Show)

instance Num Q where
  Q a + Q b = Q (a + b)
  Q a * Q b = Q (a * b)
  negate (Q a) = Q (negate a)
  abs (Q a) = Q (abs a)
  signum (Q a) = Q (signum a)
  fromInteger n = Q (fromInteger n)

instance Fractional Q where
  Q a / Q b = Q (a / b)
  fromRational r = Q r

seven :: Num a => a
seven = 7

half :: Fractional a => a
half = 0.5

isZero :: (Eq a, Num a) => a -> Bool
isZero 0 = True
isZero _ = False

isHalf :: Double -> Bool
isHalf 0.5 = True
isHalf _ = False

main :: IO ()
main = do
  print ("integer literal", 42 :: Int, 42 :: Integer, 42 :: Double, 42 :: Float)
  print ("integer literal at Rational", 42 :: Rational)
  print ("fractional literal", 1.5 :: Double, 1.5 :: Float, 1.5 :: Rational)
  print ("exact rational from decimal", 0.1 :: Rational, 0.125 :: Rational, 3.14 :: Rational)
  print ("exponent", 1e3 :: Double, 1.5e-2 :: Double, 2E2 :: Double, 1e+2 :: Double, 12.5e1 :: Double)
  print ("exponent at Rational", 1e3 :: Rational, 1.5e-2 :: Rational, 25e-1 :: Rational)
  print ("hexadecimal", 0xFF :: Int, 0Xff :: Int, 0xdeadBEEF :: Integer, 0x0 :: Int)
  print ("octal", 0o17 :: Int, 0O17 :: Int, 0o777 :: Integer)
  print ("hex/octal at Double", 0x10 :: Double, 0o10 :: Double)
  print ("big", 123456789012345678901234567890 :: Integer, 0xFFFFFFFFFFFFFFFFFFFF :: Integer)
  print ("leading zeros", 007 :: Int, 00.50 :: Double, 0e0 :: Double)
  print ("negation is negate of the literal", -1 :: Int, negate 1 :: Int, -2.5 :: Double)
  print ("user Num via fromInteger", 10 :: Mod7, 3 + 5 :: Mod7, -1 :: Mod7, 2 * 4 * 6 :: Mod7)
  print ("user Fractional via fromRational", 0.75 :: Q, 2 :: Q, 1.5e1 :: Q, 0.5 + 0.25 :: Q)
  print ("defaulting to Integer", 2 ^ 70, 7 `div` 2)
  print ("defaulting to Double", 1 / 4, 2.5 * 2)
  print ("polymorphic constants", seven :: Int, seven :: Double, seven :: Rational, seven :: Mod7)
  print ("polymorphic fraction", half :: Double, half :: Float, half :: Rational, half :: Q)
  print ("literal patterns", isZero (0 :: Int), isZero (0.0 :: Double), isZero (7 :: Mod7), isZero (3 :: Mod7), isHalf 0.5)
  print ("same literal, two types", (1 :: Int) + 1, (1 :: Double) + 1, fromIntegral (3 :: Int) + 0.5 :: Double)
  print ("float literal precision", 0.1 :: Float, 0.1 :: Double, 16777217 :: Float, 9007199254740993 :: Double)
