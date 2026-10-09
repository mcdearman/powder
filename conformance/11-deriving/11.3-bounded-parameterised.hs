-- Haskell 2010 Report 11.3: derived Bounded for parameterised single-constructor types needs Bounded for the parameters
module Main (main) where

data Pair a b = Pair a b deriving (Eq, Ord, Show, Bounded)

data Triple a = Triple a a a deriving (Eq, Show, Bounded)

data Box a = Box {content :: a} deriving (Eq, Show, Bounded)

newtype Id a = Id a deriving (Eq, Show, Bounded)

-- parameter used inside a tuple
data Both a b = Both (a, b) Bool deriving (Eq, Show, Bounded)

-- phantom parameter: nothing is required of it
data Tagged t = Tagged Bool deriving (Eq, Show, Bounded)

lo :: (Bounded a, Bounded b) => Pair a b
lo = minBound

hi :: (Bounded a, Bounded b) => Pair a b
hi = maxBound

range :: Bounded a => (Triple a, Triple a)
range = (minBound, maxBound)

fmapFst :: (a -> c) -> Pair a b -> Pair c b
fmapFst f (Pair a b) = Pair (f a) b

main :: IO ()
main = do
  print ("Pair Bool Ordering", lo :: Pair Bool Ordering, hi :: Pair Bool Ordering)
  print ("Pair Char ()", fmapFst fromEnum (lo :: Pair Char ()), fmapFst fromEnum (hi :: Pair Char ()))
  print ("Pair Int Bool", (lo :: Pair Int Bool) == Pair minBound False, (hi :: Pair Int Bool) == Pair maxBound True)
  print ("Pair nested", minBound :: Pair (Pair Bool Bool) Ordering, maxBound :: Pair (Pair Bool Bool) Ordering)
  print ("Triple", range :: (Triple Bool, Triple Bool), fst (range :: (Triple Ordering, Triple Ordering)))
  print ("Box", minBound :: Box Bool, maxBound :: Box Ordering, content (maxBound :: Box (Bool, Bool)))
  print ("Id", minBound :: Id Bool, maxBound :: Id (Id Ordering))
  print ("Both", minBound :: Both Bool Ordering, maxBound :: Both () Bool)
  print ("Tagged with a non-Bounded parameter", minBound :: Tagged Integer, maxBound :: Tagged [Int])
  print ("with tuples", minBound :: (Pair Bool Bool, Id ()), maxBound :: Pair (Bool, Bool) (Ordering, ()))
  print ("ordering of bounds", (lo :: Pair Bool Ordering) < hi, minimum [hi, lo, Pair True LT] == lo)
