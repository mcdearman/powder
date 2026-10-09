-- Haskell 2010 Report 4.3.4: defaulting with Show, Eq, Ord, Enum, Integral, RealFrac, Floating and Real constraints
module Main (main) where

main :: IO ()
main = do
  print ("Show+Num", show 42)
  print ("Enum from", take 3 [10 ..])
  print ("Enum step", [1, 3 .. 9])
  print ("Enum succ", succ 41, pred 0)
  print ("Enum fractional", [1.0, 1.5 .. 3.0])
  print ("Integral quot rem", 100 `quot` 7, 100 `rem` 7)
  print ("Integral divMod", (-7) `divMod` 2)
  print ("Integral no overflow", 2 ^ 62 + 2 ^ 62)
  print ("RealFrac floor ceiling", floor 2.5, ceiling 2.5)
  print ("RealFrac round", round 2.5, round 3.5, round (-2.5))
  print ("RealFrac properFraction", properFraction 3.75)
  print ("Floating", pi > 3, sqrt 2 * sqrt 2 > 1.9)
  print ("Floating exact", exp 0, sqrt 4)
  print ("Ord+Num", max 3 4, compare 1 2)
  print ("Eq+Num", 2 + 2 == 4, 0.5 + 0.5 /= 1)
  print ("Real", toRational 0.5)
  print ("Read+Num", read "5" * 2)
  print ("let-bound", let k = 3 in k * k)
  let side = 12
      area = side * side
  print ("do-let-bound", area)
  print ("where-bound", volume)

volume :: String
volume = show cube
  where
    cube = edge * edge * edge
    edge = 2 ^ 22
