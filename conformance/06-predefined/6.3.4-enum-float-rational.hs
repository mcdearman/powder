-- Haskell 2010 Report 6.3.4: Enum at Float and at Ratio Integer follows the same numericEnumFrom rules as Double
import Data.Ratio ((%))

fs :: [Float] -> [Float]
fs = id

rs :: [Rational] -> [Rational]
rs = id

main :: IO ()
main = do
  print ("float succ/pred", succ (1.5 :: Float), pred (1.5 :: Float))
  print ("float toEnum/fromEnum", toEnum 3 :: Float, fromEnum (3.7 :: Float), fromEnum (-3.7 :: Float))
  print ("float [1.0..2.5]", fs [1.0 .. 2.5])
  print ("float [1.0..2.4]", fs [1.0 .. 2.4])
  print ("float [1.0..0.5]", fs [1.0 .. 0.5])
  print ("float [2.0..1.0]", fs [2.0 .. 1.0])
  print ("float [1.0,1.5..3.25]", fs [1.0, 1.5 .. 3.25])
  print ("float [1.0,1.5..3.1]", fs [1.0, 1.5 .. 3.1])
  print ("float [5.0,3.0..0.0]", fs [5.0, 3.0 .. 0.0])
  print ("float [4.0,3.75..3.0]", fs [4.0, 3.75 .. 3.0])
  print ("float enumFrom", take 3 (fs [0.25 ..]), take 3 (fs [0.0, -0.5 ..]))
  print ("float length [0.0,0.1..1.0]", length (fs [0.0, 0.1 .. 1.0]))
  print ("rational succ/pred", succ (1 % 2 :: Rational), pred (1 % 2 :: Rational))
  print ("rational toEnum/fromEnum", toEnum 3 :: Rational, fromEnum (7 % 2 :: Rational), fromEnum ((-7) % 2 :: Rational))
  print ("rational [1%2..3]", rs [1 % 2 .. 3])
  print ("rational [1..5%2]", rs [1 .. 5 % 2])
  print ("rational [1..12%5]", rs [1 .. 12 % 5])
  print ("rational [1,3%2..3]", rs [1, 3 % 2 .. 3])
  print ("rational [0,1%3..1]", rs [0, 1 % 3 .. 1])
  print ("rational [0,1%10..1] exact", length (rs [0, 1 % 10 .. 1]), last (rs [0, 1 % 10 .. 1]))
  print ("rational [1,2%3..0]", rs [1, 2 % 3 .. 0])
  print ("rational [1,1%2..(-1)%4]", rs [1, 1 % 2 .. (-1) % 4])
  print ("rational enumFrom", take 3 (rs [1 % 3 ..]), take 3 (rs [1, 3 % 4 ..]))
