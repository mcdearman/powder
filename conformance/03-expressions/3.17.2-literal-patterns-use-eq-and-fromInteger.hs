-- Haskell 2010 Report 3.17.2: a numeric literal pattern k matches v iff v == k, using the type's own (==), fromInteger / fromRational and negate
module Main (main) where

newtype Mod10 = Mod10 Int
  deriving Show

instance Eq Mod10 where
  Mod10 a == Mod10 b = a `mod` 10 == b `mod` 10

instance Num Mod10 where
  Mod10 a + Mod10 b = Mod10 (a + b)
  Mod10 a - Mod10 b = Mod10 (a - b)
  Mod10 a * Mod10 b = Mod10 (a * b)
  negate (Mod10 a) = Mod10 (negate a)
  abs (Mod10 a) = Mod10 (abs a)
  signum (Mod10 a) = Mod10 (signum a)
  fromInteger k = Mod10 (fromInteger k)

instance Fractional Mod10 where
  Mod10 a / Mod10 b = Mod10 (a `div` b)
  fromRational r = Mod10 (truncate r + 5)

data Always = Always
  deriving Show

instance Eq Always where
  _ == _ = True

instance Num Always where
  _ + _ = Always
  _ - _ = Always
  _ * _ = Always
  negate _ = Always
  abs _ = Always
  signum _ = Always
  fromInteger _ = Always

f :: Mod10 -> String
f 3 = "three-ish"
f 7 = "seven-ish"
f _ = "other"

g :: Mod10 -> String
g (-3) = "minus-three-ish"
g _ = "other"

h :: Mod10 -> String
h 1.5 = "fromRational 1.5 is Mod10 6"
h _ = "other"

always :: Always -> String
always 99 = "first literal clause always matches"
always _ = "unreachable"

main :: IO ()
main = do
  print ("exact", f (Mod10 3), f (Mod10 7), f (Mod10 4))
  print ("via user ==", f (Mod10 13), f (Mod10 107), f (Mod10 (-3)))
  print ("negative literal via negate", g (Mod10 7), g (Mod10 (-3)), g (Mod10 3))
  print ("float literal via fromRational", h (Mod10 6), h (Mod10 16), h (Mod10 1))
  print ("always equal", always Always)
  print ("in case", case Mod10 23 of { 3 -> "matched 3"; _ -> "no" })
  print ("in tuple", case (Mod10 12, 'a') of { (2, 'a') -> "matched"; _ -> "no" })
  print ("standard types", case (2 :: Integer, 2.0 :: Double, 2 :: Rational) of { (2, 2, 2) -> "all equal 2"; _ -> "no" })
