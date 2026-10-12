-- Num has no classes above it, as in GHC and not as in the Report: a type that can be neither compared nor shown
-- may be a number, and a function over numbers that compares or shows them says so in its type
newtype Poly = Poly [Int]

instance Num Poly where
  Poly a + Poly b = Poly (zipLong a b)
    where
      zipLong (x : xs) (y : ys) = (x + y) : zipLong xs ys
      zipLong xs [] = xs
      zipLong [] ys = ys
  Poly a * Poly b = Poly [sum [a !! i * b !! (k - i) | i <- [0 .. k], i < length a, k - i < length b] | k <- [0 .. length a + length b - 2]]
  negate (Poly a) = Poly (map negate a)
  abs = id
  signum = id
  fromInteger n = Poly [fromInteger n]

coefficients :: Poly -> [Int]
coefficients (Poly a) = a

-- Functions are numbers, pointwise.
newtype Fn = Fn (Int -> Int)

instance Num Fn where
  Fn f + Fn g = Fn (\x -> f x + g x)
  Fn f * Fn g = Fn (\x -> f x * g x)
  negate (Fn f) = Fn (negate . f)
  abs (Fn f) = Fn (abs . f)
  signum (Fn f) = Fn (signum . f)
  fromInteger n = Fn (const (fromInteger n))

apply :: Fn -> Int -> Int
apply (Fn f) = f

double :: Num a => a -> a
double x = x + x

isZero :: (Eq a, Num a) => a -> Bool
isZero 0 = True
isZero _ = False

describe :: (Show a, Num a) => a -> String
describe x = show (x * 2)

main :: IO ()
main = do
  let x = Poly [0, 1]
  putStrLn ("poly: " ++ show (coefficients ((x + 1) * (x + 1)), coefficients (double x), coefficients (x * x - 1)))
  putStrLn ("poly-literal: " ++ show (coefficients 7, coefficients (sum [x, x, 3])))
  putStrLn ("fn: " ++ show (apply (Fn (* 2) + 1) 5, apply (double (Fn succ)) 1, apply (product [Fn id, Fn id, 2]) 3))
  putStrLn ("constrained: " ++ show (isZero (0 :: Int), isZero (1.5 :: Double), describe (3 :: Integer), describe (0.5 :: Double)))
  putStrLn ("ratio-power: " ++ show (double (2 :: Int) ^ (3 :: Int), (2 :: Double) ^^ (-1 :: Int)))
