-- Haskell 2010 Report 4.3.2: user instances of Enum and Bounded; class defaults for succ, pred and bounded ranges
module Main (main) where

data Day = Mon | Tue | Wed | Thu | Fri deriving (Eq, Show)

instance Enum Day where
  fromEnum Mon = 0
  fromEnum Tue = 1
  fromEnum Wed = 2
  fromEnum Thu = 3
  fromEnum Fri = 4
  toEnum 0 = Mon
  toEnum 1 = Tue
  toEnum 2 = Wed
  toEnum 3 = Thu
  toEnum _ = Fri

instance Bounded Day where
  minBound = Mon
  maxBound = Fri

-- even numbers only: position n stands for 2 * n
newtype Even = Even Int deriving (Eq, Show)

instance Enum Even where
  toEnum n = Even (2 * n)
  fromEnum (Even n) = n `div` 2

instance Bounded Even where
  minBound = Even 0
  maxBound = Even 10

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. maxBound]

main :: IO ()
main = do
  print ("fromEnum", map fromEnum [Mon, Tue, Wed, Thu, Fri])
  print ("toEnum", toEnum 2 :: Day)
  print ("succ default", succ Mon, succ Thu)
  print ("pred default", pred Fri, pred Tue)
  print ("enumFromTo default", [Mon .. Thu])
  print ("enumFromTo single", [Wed .. Wed])
  print ("enumFromTo empty", [Thu .. Tue])
  print ("enumFromThenTo default", [Mon, Wed .. Fri])
  print ("enumFromThenTo down", [Fri, Thu .. Mon])
  print ("bounds", minBound :: Day, maxBound :: Day)
  print ("all days", allValues :: [Day])
  print ("comprehension", [(d, fromEnum d) | d <- [Tue .. Wed]])
  print ("even range", [Even 0 .. Even 6])
  print ("even step", [Even 0, Even 4 .. Even 10])
  print ("even succ pred", succ (Even 4), pred (Even 4))
  print ("all evens", allValues :: [Even])
