-- Haskell 2010 Report 6.3.7: class Bounded; minBound and maxBound at Int, Char, Bool, Ordering, () and tuples
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show, Bounded, Enum)

newtype Percent = Percent Int deriving (Eq, Show)

instance Bounded Percent where
  minBound = Percent 0
  maxBound = Percent 100

data Both = Both Bool Ordering deriving (Eq, Show, Bounded)

range :: Bounded a => (a, a)
range = (minBound, maxBound)

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. maxBound]

main :: IO ()
main = do
  print ("Int", minBound :: Int, maxBound :: Int)
  print ("Int values", (minBound :: Int) == negate (2 ^ (63 :: Int)), (maxBound :: Int) == 2 ^ (63 :: Int) - 1)
  print ("Int as Integer", toInteger (minBound :: Int), toInteger (maxBound :: Int) + 1)
  print ("Char", minBound :: Char, maxBound :: Char, fromEnum (minBound :: Char), fromEnum (maxBound :: Char))
  print ("Bool", minBound :: Bool, maxBound :: Bool)
  print ("Ordering", minBound :: Ordering, maxBound :: Ordering)
  print ("unit", minBound :: (), maxBound :: ())
  print ("pair", minBound :: (Bool, Ordering), maxBound :: (Bool, Ordering))
  print ("triple", minBound :: (Bool, (), Ordering), maxBound :: (Bool, (), Ordering))
  print ("nested tuple", maxBound :: ((Bool, Bool), (Ordering, ())))
  print ("tuple with Int and Char", (minBound :: (Int, Char)) == (minBound, minBound), maxBound :: (Bool, Int))
  print ("polymorphic use", range :: (Bool, Bool), range :: (Ordering, Ordering), range :: ((), ()))
  print ("derived enumeration", minBound :: Suit, maxBound :: Suit)
  print ("derived single-constructor product", minBound :: Both, maxBound :: Both)
  print ("user instance", minBound :: Percent, maxBound :: Percent)
  print ("user type inside a tuple", maxBound :: (Percent, Suit))
  print ("all values", allValues :: [Bool], allValues :: [Ordering], allValues :: [Suit], allValues :: [()])
  print ("ordering of bounds", (minBound :: Int) < maxBound, (minBound :: Char) < maxBound, (minBound :: Bool) < maxBound)
  print ("bound not exceeded", all (<= (maxBound :: Char)) "az\1114111", all (>= (minBound :: Int)) [0, -1, minBound])
