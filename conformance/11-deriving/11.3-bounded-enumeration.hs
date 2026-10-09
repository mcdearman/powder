-- Haskell 2010 Report 11.3: derived Bounded for an enumeration: minBound is the first constructor, maxBound the last
module Main (main) where

-- declaration order matters, not the names
data Size = Medium | Tiny | Huge | Small deriving (Eq, Ord, Show, Enum, Bounded)

-- Bounded alone can be derived
data Dir = North | East | South | West deriving (Bounded)

data Only = Only deriving (Show, Bounded)

dirName :: Dir -> String
dirName North = "North"
dirName East = "East"
dirName South = "South"
dirName West = "West"

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. maxBound]

cyclicSucc :: (Eq a, Bounded a, Enum a) => a -> a
cyclicSucc x = if x == maxBound then minBound else succ x

main :: IO ()
main = do
  print ("minBound is the first constructor", minBound :: Size)
  print ("maxBound is the last constructor", maxBound :: Size)
  print ("bounds agree with Ord and Enum", minBound == minimum [Tiny, Small, Medium, Huge], maxBound == maximum [Tiny, Small, Medium, Huge], fromEnum (maxBound :: Size))
  print ("[minBound .. maxBound]", [minBound .. maxBound :: Size])
  print ("allValues", allValues :: [Size], length (allValues :: [Size]))
  print ("[minBound ..]", [minBound ..] :: [Size], [maxBound, pred maxBound ..] :: [Size])
  print ("every value within bounds", all (\x -> minBound <= x && x <= maxBound) (allValues :: [Size]))
  print ("cyclicSucc", map cyclicSucc (allValues :: [Size]), take 6 (iterate cyclicSucc Huge))
  print ("Bounded without Eq/Show", dirName minBound, dirName maxBound)
  print ("single constructor", minBound :: Only, maxBound :: Only)
  print ("in tuples", minBound :: (Size, Bool), maxBound :: (Only, Size, Ordering))
  print ("asTypeOf", minBound `asTypeOf` Huge, maxBound `asTypeOf` Tiny)
  print ("generic allValues at Prelude types", allValues :: [Bool], allValues :: [Ordering], allValues :: [()])
