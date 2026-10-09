-- Haskell 2010 Report 11.3: derived Bounded for a type with a single constructor: the bounds are the constructor
-- applied to the bounds of the field types
module Main (main) where

data P = P Bool Ordering deriving (Eq, Ord, Show, Bounded)

data Q = Q Int Char () deriving (Eq, Show, Bounded)

-- record syntax
data Cfg = Cfg {verbose :: Bool, level :: Ordering, tag :: ()} deriving (Eq, Show, Bounded)

-- fields that are themselves derived Bounded types
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Show, Enum, Bounded)

data Card = Card Suit P deriving (Eq, Ord, Show, Bounded)

-- fields of tuple type
data T = T (Bool, Suit) (Ordering, (), Bool) deriving (Eq, Show, Bounded)

-- infix constructor
data I = Bool :& Suit deriving (Eq, Show, Bounded)

-- single field
data W = W Suit deriving (Eq, Show, Bounded)

main :: IO ()
main = do
  print ("P", minBound :: P, maxBound :: P)
  print ("P equals", (minBound :: P) == P False LT, (maxBound :: P) == P True GT)
  print ("P bounds are extreme", all (\x -> minBound <= x && x <= maxBound) [P b o | b <- [False, True], o <- [LT, EQ, GT]])
  print ("Q", (minBound :: Q) == Q minBound minBound (), (maxBound :: Q) == Q maxBound maxBound ())
  print ("Q fields", (\(Q n c u) -> (n, fromEnum c, u)) minBound, (\(Q n c u) -> (n, fromEnum c, u)) maxBound)
  print ("Cfg", minBound :: Cfg, maxBound :: Cfg)
  print ("Cfg fields", verbose minBound, level maxBound, tag (minBound :: Cfg))
  print ("Suit", minBound :: Suit, maxBound :: Suit)
  print ("Card", minBound :: Card, maxBound :: Card)
  print ("T", minBound :: T, maxBound :: T)
  print ("I", minBound :: I, maxBound :: I)
  print ("W", minBound :: W, maxBound :: W)
  print ("nested in tuples", minBound :: (P, W), maxBound :: (Suit, (P, Bool)))
  print ("asTypeOf", minBound `asTypeOf` P True EQ, maxBound `asTypeOf` Card Hearts (P False EQ))
