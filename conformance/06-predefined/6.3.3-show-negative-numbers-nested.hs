-- Haskell 2010 Report 6.3.3: show of negative Int/Integer/Rational inside data structures (parenthesised iff precedence > 6)
import Data.Ratio ((%))

data P = P Int Integer deriving Show

data R = R { field :: Int, other :: Integer } deriving Show

infixl 6 :+

data C = Int :+ Int deriving Show

infixr 5 :>

data L = Int :> L | End deriving Show

newtype N = N Int deriving Show

main :: IO ()
main = do
  print (-3 :: Int)
  print (Just (-3 :: Int))
  print [-1, 2, -3 :: Int]
  print (-1 :: Int, -2 :: Integer)
  print (Left (-1) :: Either Int Int)
  print (Right (-1) :: Either Int Integer)
  print (Just (Just (-1 :: Int)))
  print [Just (-1 :: Int), Nothing]
  print (Just [-1 :: Int])
  print (Just (-1 :: Int, -2 :: Int))
  print [[-1], [], [2, -2 :: Integer]]
  print (P (-1) (-2))
  print (P 1 2)
  print (Just (P (-1) 2))
  print (R { field = -1, other = -2 })
  print [R { field = 1, other = -2 }]
  print ((-1) :+ (-2))
  print (1 :+ 2)
  print (Just ((-1) :+ 2))
  print ((-1) :> 2 :> (-3) :> End)
  print (N (-5))
  print (Just (N (-5)))
  print ((-1) % 2 :: Rational)
  print (Just (1 % 2 :: Rational))
  print (Just ((-1) % 2 :: Rational))
  print [1 % 2, (-3) % 4 :: Rational]
  print (1 % 2 :: Rational, (-3) % 4 :: Rational)
  print (Just (-12345678901234567890 :: Integer))
  print (minBound :: Int, Just (minBound :: Int))
