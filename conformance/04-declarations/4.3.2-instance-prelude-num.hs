-- Haskell 2010 Report 4.3.2: user instances of the Prelude class Num (literals, defaults for - and negate)
module Main (main) where

data V2 = V2 Int Int deriving (Eq, Show)

instance Num V2 where
  V2 a b + V2 c d = V2 (a + c) (b + d)
  V2 a b * V2 c d = V2 (a * c) (b * d)
  negate (V2 a b) = V2 (negate a) (negate b)
  abs (V2 a b) = V2 (abs a) (abs b)
  signum (V2 a b) = V2 (signum a) (signum b)
  fromInteger n = V2 (fromInteger n) (fromInteger n)

-- defines (-) and leaves negate to the default (negate x = 0 - x)
newtype Z = Z Int deriving (Eq, Show)

instance Num Z where
  Z a + Z b = Z (a + b)
  Z a - Z b = Z (a - b)
  Z a * Z b = Z (a * b)
  abs (Z a) = Z (abs a)
  signum (Z a) = Z (signum a)
  fromInteger n = Z (fromInteger n)

sumSquares :: Num a => [a] -> a
sumSquares xs = sum (map (\x -> x * x) xs)

main :: IO ()
main = do
  print ("add", V2 1 2 + V2 3 4)
  print ("mul", V2 1 2 * V2 3 4)
  print ("literal", 7 :: V2)
  print ("literal operand", V2 1 2 * 3, 10 + V2 1 2)
  print ("default minus", V2 5 5 - V2 1 2)
  print ("negate", negate (V2 1 (-2)))
  print ("unary minus", - V2 1 2)
  print ("abs signum", abs (V2 (-1) 2), signum (V2 (-5) 0))
  print ("sum", sum [V2 1 1, V2 2 2, V2 3 3])
  print ("product", product [V2 1 2, V2 3 4])
  print ("power", V2 2 3 ^ (3 :: Int))
  print ("subtract", subtract 1 (V2 5 6))
  print ("precedence", V2 1 1 + 2 * V2 3 4)
  print ("generic function", sumSquares [V2 1 2, V2 3 4], sumSquares [1, 2, 3 :: Int])
  print ("z minus", Z 5 - Z 7)
  print ("z default negate", negate (Z 3), - Z 4)
  print ("z literal", 3 + Z 4 * 2)
  print ("z sum", sumSquares [Z 1, Z 2, Z 3])
