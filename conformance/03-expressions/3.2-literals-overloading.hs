-- Haskell 2010 Report 3.2: integer literal = fromInteger applied to an Integer; float literal = fromRational applied to a Rational
module Main (main) where

data Lit = FromI Integer | FromR Rational | Other
  deriving (Eq, Show)

instance Num Lit where
  _ + _ = Other
  _ - _ = Other
  _ * _ = Other
  negate _ = Other
  abs _ = Other
  signum _ = Other
  fromInteger n = FromI n

instance Fractional Lit where
  _ / _ = Other
  fromRational r = FromR r

main :: IO ()
main = do
  print ("integer literal", 2 :: Lit)
  print ("zero", 0 :: Lit)
  print ("hex", 0x10 :: Lit, 0XfF :: Lit)
  print ("octal", 0o17 :: Lit, 0O10 :: Lit)
  print ("big", 123456789012345678901234567890 :: Lit)
  print ("float literal", 1.5 :: Lit)
  print ("float whole", 2.0 :: Lit)
  print ("exponent", 1e2 :: Lit, 1.0e-2 :: Lit)
  print ("exact decimal", 0.1 :: Lit, 0.125 :: Lit)
  print ("capital E", 25E-1 :: Lit, 1.5e+1 :: Lit)
  print ("arith", (1 + 2) :: Lit)
  print ("at Int", 3 :: Int)
  print ("at Integer", 3 :: Integer)
  print ("at Double", 3 :: Double)
  print ("at Float", 3 :: Float)
  print ("at Rational", 3 :: Rational, 2.5 :: Rational, 0.1 :: Rational)
  print ("float at Double", 2.5 :: Double, 1e3 :: Double, 1.0e-2 :: Double)
  print ("wrap", 18446744073709551617 :: Int)
  print ("by context", length "abc" + 1, 2 * (1.5 :: Double))
  print ("defaulted", 7, 7.5)
