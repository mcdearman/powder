-- Haskell 2010 Report 3.10: arithmetic sequences translate to enumFrom, enumFromThen, enumFromTo, enumFromThenTo of the element type
module Main (main) where

data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet
  deriving (Eq, Ord, Show, Enum, Bounded)

-- A deliberately odd instance, so that each sequence form shows which method it calls.
newtype Step = Step Int
  deriving (Eq, Show)

instance Enum Step where
  toEnum n = Step n
  fromEnum (Step n) = n
  enumFrom (Step n) = [Step n, Step (n + 100)]
  enumFromThen (Step n) (Step m) = [Step (n + m)]
  enumFromTo (Step n) (Step m) = [Step m, Step n]
  enumFromThenTo (Step a) (Step b) (Step c) = [Step (a * 100 + b * 10 + c)]

main :: IO ()
main = do
  print ("from", [Red ..])
  print ("from middle", [Blue ..])
  print ("from-then", [Red, Yellow ..])
  print ("from-then down", [Violet, Blue ..])
  print ("from-then same", take 2 [Green, Green ..])
  print ("from-to", [Orange .. Blue])
  print ("from-to empty", [Blue .. Red])
  print ("from-to single", [Green .. Green])
  print ("from-then-to", [Red, Green .. Violet])
  print ("from-then-to short", [Red, Green .. Indigo])
  print ("from-then-to down", [Blue, Green .. Red])
  print ("from-then-to empty", [Red, Orange .. Violet] == [Red ..], [Orange, Yellow .. Red])
  print ("bounds", [minBound .. maxBound :: Color])
  print ("methods", [Red ..] == enumFrom Red, [Red, Blue ..] == enumFromThen Red Blue)
  print ("methods to", [Red .. Blue] == enumFromTo Red Blue, [Red, Yellow .. Blue] == enumFromThenTo Red Yellow Blue)
  print ("user enumFrom", [Step 1 ..])
  print ("user enumFromThen", [Step 1, Step 2 ..])
  print ("user enumFromTo", [Step 1 .. Step 2])
  print ("user enumFromThenTo", [Step 1, Step 2 .. Step 3])
