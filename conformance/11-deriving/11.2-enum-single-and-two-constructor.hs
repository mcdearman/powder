-- Haskell 2010 Report 11.2: derived Enum on the smallest enumerations: one constructor and two constructors
module Main (main) where

data One = One deriving (Eq, Ord, Show, Enum, Bounded)

data Switch = Off | On deriving (Eq, Ord, Show, Enum, Bounded)

main :: IO ()
main = do
  print ("One fromEnum/toEnum", fromEnum One, toEnum 0 :: One)
  print ("One enumFrom", enumFrom One, [One ..])
  print ("One enumFromTo", enumFromTo One One, [One .. One])
  print ("One enumFromThen is infinite", take 3 (enumFromThen One One), take 2 [One, One ..])
  print ("One enumFromThenTo is infinite", take 3 (enumFromThenTo One One One), take 2 [One, One .. One])
  print ("One bounds", minBound :: One, maxBound :: One, [minBound .. maxBound :: One])
  print ("One length", length [One ..], length [minBound .. maxBound :: One])
  print ("Switch fromEnum/toEnum", fromEnum Off, fromEnum On, toEnum 0 :: Switch, toEnum 1 :: Switch)
  print ("Switch succ/pred", succ Off, pred On)
  print ("Switch enumFrom", [Off ..], [On ..])
  print ("Switch enumFromThen", [Off, On ..], [On, Off ..])
  print ("Switch enumFromThen constant", take 3 [Off, Off ..], take 3 [On, On ..])
  print ("Switch enumFromTo", [Off .. On], [On .. Off], [Off .. Off], [On .. On])
  print ("Switch enumFromThenTo", [Off, On .. On], [Off, On .. Off], [On, Off .. Off], [On, Off .. On])
  print ("Switch enumFromThenTo wrong direction", take 3 [On, On .. On], [On, On .. Off])
  print ("Switch agrees with Bool", map fromEnum [Off ..] == map fromEnum [False ..], map fromEnum [On, Off ..] == map fromEnum [True, False ..])
  print ("Switch bounds", minBound :: Switch, maxBound :: Switch, [minBound .. maxBound :: Switch])
  print ("Switch cycle", take 5 (cycle [Off ..]), map (toEnum . (`mod` 2)) [0 .. 4 :: Int] :: [Switch])
