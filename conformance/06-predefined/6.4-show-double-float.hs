-- Haskell 2010 Report 6.4 / Prelude showFloat: show of Double and Float; fixed notation for 0.1 <= |x| < 10^7,
-- otherwise d.ddde<n>; shortest digits that uniquely identify the value; NaN, Infinity, -0.0
d :: Double -> String
d = show

f :: Float -> String
f = show

main :: IO ()
main = do
  print ("whole numbers", d 0, d 1, d 3, d 100, d (-2))
  print ("fractions", d 0.5, d 0.25, d 1.5, d 12345.678, d (-0.75))
  print ("lower switch point", d 0.1, d 0.11, d 0.09, d 0.099, d 1.0e-2, d 5.0e-2)
  print ("upper switch point", d 999999, d 9999999, d 1.0e7, d 12345678, d 1234567.8, d 9999999.5)
  print ("large", d 1.0e10, d 1.5e10, d 1.0e21, d 1.0e22, d 1.0e100, d 6.02214076e23)
  print ("small", d 1.0e-3, d 1.5e-3, d 1.0e-10, d 1.0e-100, d 6.62607015e-34)
  print ("shortest uniquely identifying digits", d (0.1 + 0.2), d (1 / 3), d (2 / 3), d (1 / 10), d (0.1 * 3))
  print ("many digits", d 3.141592653589793, d 2.718281828459045, d 0.30000000000000004, d 1.0000000000000002)
  print ("powers of two", d 1024, d 4294967296, d (2 ^ (53 :: Int)), d (2 ^ (64 :: Int)), d (2 ^^ (-10 :: Int)))
  print ("extremes", d 1.7976931348623157e308, d 2.2250738585072014e-308, d 5.0e-324)
  print ("special values", d (0 / 0), d (1 / 0), d (-1 / 0), d (-0.0), d (negate 0))
  print ("negative", d (-1.5), d (-1.0e-2), d (-1.0e7), d (-123.456))
  print ("lists and tuples", show [1.0, 2.5 :: Double], show (1.5 :: Double, 0.1 :: Double), show [0 / 0, 1 / 0 :: Double])
  print ("negative in a list or tuple", show [-1.5, 2 :: Double], show (-1.5 :: Double, -2.0e-3 :: Double))
  print ("print forms", 1.0e-2 :: Double, 1.0e7 :: Double, 0.1 :: Double, 12345.678 :: Double)
  print ("float whole and fractions", f 0, f 1, f 0.5, f 0.1, f 3.14, f (-2.5))
  print ("float switch points", f 1.0e-2, f 9.9e-2, f 9999999, f 1.0e7, f 1.0e8)
  print ("float shortest digits", f (1 / 3), f (2 / 3), f (0.1 + 0.2), f 16777216, f 16777217)
  print ("float extremes", f 3.4028235e38, f 1.17549435e-38, f 1.0e-45)
  print ("float special values", f (0 / 0), f (1 / 0), f (-1 / 0), f (-0.0))
  print ("float vs double of the same literal", f 0.1 == d 0.1, f (1 / 3) == d (1 / 3), show (realToFrac (0.1 :: Float) :: Double))
  print ("shows appends", shows (1.5 :: Double) "x", shows (2.0e-2 :: Float) "y", showsPrec 0 (-1.5 :: Double) "")
  print ("read . show", all (\x -> read (show x) == x) [0.1, 1 / 3, 1.0e-2, 1.0e7, 123456.789, 5.0e-324, 1.7976931348623157e308 :: Double])
