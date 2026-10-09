-- Whole numbers of a fixed size, which wrap round. Each is an Integer
-- kept within its range.
module Data.Int (Int8, Int16, Int32, Int64) where

import Data.Bits
import Data.Ix

newtype Int8 = MkInt8 Integer

-- The number in range that an integer is the same as, counting round.
toInt8 :: Integer -> Int8
toInt8 x = MkInt8 (mod (x + 128) 256 - 128)

instance Eq Int8 where
  MkInt8 a == MkInt8 b = a == b

instance Ord Int8 where
  compare (MkInt8 a) (MkInt8 b) = compare a b
  MkInt8 a <= MkInt8 b = a <= b
  MkInt8 a < MkInt8 b = a < b

instance Show Int8 where
  showsPrec p (MkInt8 a) = showsPrec p a

instance Read Int8 where
  readsPrec p s = [(toInt8 x, t) | (x, t) <- readsPrec p s]

instance Num Int8 where
  MkInt8 a + MkInt8 b = toInt8 (a + b)
  MkInt8 a - MkInt8 b = toInt8 (a - b)
  MkInt8 a * MkInt8 b = toInt8 (a * b)
  negate (MkInt8 a) = toInt8 (negate a)
  abs (MkInt8 a) = toInt8 (abs a)
  signum (MkInt8 a) = MkInt8 (signum a)
  fromInteger = toInt8

instance Real Int8 where
  toRational (MkInt8 a) = toRational a

instance Enum Int8 where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toInt8 (toInteger n)
  fromEnum (MkInt8 a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkInt8 a) (MkInt8 b) = map MkInt8 (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkInt8 a) (MkInt8 b) (MkInt8 d) = map MkInt8 (enumFromThenTo a b d)

instance Integral Int8 where
  quot (MkInt8 a) (MkInt8 b) = toInt8 (quot a b)
  rem (MkInt8 a) (MkInt8 b) = toInt8 (rem a b)
  div (MkInt8 a) (MkInt8 b) = toInt8 (div a b)
  mod (MkInt8 a) (MkInt8 b) = toInt8 (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkInt8 a) = a

instance Bounded Int8 where
  minBound = MkInt8 (negate 128)
  maxBound = MkInt8 127

instance Ix Int8 where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromInteger (toInteger i - toInteger m) else error "Ix.index: index out of range"
  inRange (m, n) i = m <= i && i <= n

instance Bits Int8 where
  MkInt8 a .&. MkInt8 b = toInt8 (a .&. b)
  MkInt8 a .|. MkInt8 b = toInt8 (a .|. b)
  xor (MkInt8 a) (MkInt8 b) = toInt8 (xor a b)
  complement (MkInt8 a) = toInt8 (complement a)
  shiftL (MkInt8 a) n = toInt8 (shiftL a n)
  shiftR (MkInt8 a) n = toInt8 (shiftR a n)
  bit n = toInt8 (bit n)
  testBit (MkInt8 a) n = testBit a n
  rotate (MkInt8 a) n = let k = mod n 8; u = mod a 256 in toInt8 (u * 2 ^ k + div u (2 ^ (8 - k)))
  bitSize _ = 8
  isSigned _ = True
  popCount (MkInt8 a) = popCount (if a < 0 then a + 256 else a)

newtype Int16 = MkInt16 Integer

-- The number in range that an integer is the same as, counting round.
toInt16 :: Integer -> Int16
toInt16 x = MkInt16 (mod (x + 32768) 65536 - 32768)

instance Eq Int16 where
  MkInt16 a == MkInt16 b = a == b

instance Ord Int16 where
  compare (MkInt16 a) (MkInt16 b) = compare a b
  MkInt16 a <= MkInt16 b = a <= b
  MkInt16 a < MkInt16 b = a < b

instance Show Int16 where
  showsPrec p (MkInt16 a) = showsPrec p a

instance Read Int16 where
  readsPrec p s = [(toInt16 x, t) | (x, t) <- readsPrec p s]

instance Num Int16 where
  MkInt16 a + MkInt16 b = toInt16 (a + b)
  MkInt16 a - MkInt16 b = toInt16 (a - b)
  MkInt16 a * MkInt16 b = toInt16 (a * b)
  negate (MkInt16 a) = toInt16 (negate a)
  abs (MkInt16 a) = toInt16 (abs a)
  signum (MkInt16 a) = MkInt16 (signum a)
  fromInteger = toInt16

instance Real Int16 where
  toRational (MkInt16 a) = toRational a

instance Enum Int16 where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toInt16 (toInteger n)
  fromEnum (MkInt16 a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkInt16 a) (MkInt16 b) = map MkInt16 (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkInt16 a) (MkInt16 b) (MkInt16 d) = map MkInt16 (enumFromThenTo a b d)

instance Integral Int16 where
  quot (MkInt16 a) (MkInt16 b) = toInt16 (quot a b)
  rem (MkInt16 a) (MkInt16 b) = toInt16 (rem a b)
  div (MkInt16 a) (MkInt16 b) = toInt16 (div a b)
  mod (MkInt16 a) (MkInt16 b) = toInt16 (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkInt16 a) = a

instance Bounded Int16 where
  minBound = MkInt16 (negate 32768)
  maxBound = MkInt16 32767

instance Ix Int16 where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromInteger (toInteger i - toInteger m) else error "Ix.index: index out of range"
  inRange (m, n) i = m <= i && i <= n

instance Bits Int16 where
  MkInt16 a .&. MkInt16 b = toInt16 (a .&. b)
  MkInt16 a .|. MkInt16 b = toInt16 (a .|. b)
  xor (MkInt16 a) (MkInt16 b) = toInt16 (xor a b)
  complement (MkInt16 a) = toInt16 (complement a)
  shiftL (MkInt16 a) n = toInt16 (shiftL a n)
  shiftR (MkInt16 a) n = toInt16 (shiftR a n)
  bit n = toInt16 (bit n)
  testBit (MkInt16 a) n = testBit a n
  rotate (MkInt16 a) n = let k = mod n 16; u = mod a 65536 in toInt16 (u * 2 ^ k + div u (2 ^ (16 - k)))
  bitSize _ = 16
  isSigned _ = True
  popCount (MkInt16 a) = popCount (if a < 0 then a + 65536 else a)

newtype Int32 = MkInt32 Integer

-- The number in range that an integer is the same as, counting round.
toInt32 :: Integer -> Int32
toInt32 x = MkInt32 (mod (x + 2147483648) 4294967296 - 2147483648)

instance Eq Int32 where
  MkInt32 a == MkInt32 b = a == b

instance Ord Int32 where
  compare (MkInt32 a) (MkInt32 b) = compare a b
  MkInt32 a <= MkInt32 b = a <= b
  MkInt32 a < MkInt32 b = a < b

instance Show Int32 where
  showsPrec p (MkInt32 a) = showsPrec p a

instance Read Int32 where
  readsPrec p s = [(toInt32 x, t) | (x, t) <- readsPrec p s]

instance Num Int32 where
  MkInt32 a + MkInt32 b = toInt32 (a + b)
  MkInt32 a - MkInt32 b = toInt32 (a - b)
  MkInt32 a * MkInt32 b = toInt32 (a * b)
  negate (MkInt32 a) = toInt32 (negate a)
  abs (MkInt32 a) = toInt32 (abs a)
  signum (MkInt32 a) = MkInt32 (signum a)
  fromInteger = toInt32

instance Real Int32 where
  toRational (MkInt32 a) = toRational a

instance Enum Int32 where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toInt32 (toInteger n)
  fromEnum (MkInt32 a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkInt32 a) (MkInt32 b) = map MkInt32 (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkInt32 a) (MkInt32 b) (MkInt32 d) = map MkInt32 (enumFromThenTo a b d)

instance Integral Int32 where
  quot (MkInt32 a) (MkInt32 b) = toInt32 (quot a b)
  rem (MkInt32 a) (MkInt32 b) = toInt32 (rem a b)
  div (MkInt32 a) (MkInt32 b) = toInt32 (div a b)
  mod (MkInt32 a) (MkInt32 b) = toInt32 (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkInt32 a) = a

instance Bounded Int32 where
  minBound = MkInt32 (negate 2147483648)
  maxBound = MkInt32 2147483647

instance Ix Int32 where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromInteger (toInteger i - toInteger m) else error "Ix.index: index out of range"
  inRange (m, n) i = m <= i && i <= n

instance Bits Int32 where
  MkInt32 a .&. MkInt32 b = toInt32 (a .&. b)
  MkInt32 a .|. MkInt32 b = toInt32 (a .|. b)
  xor (MkInt32 a) (MkInt32 b) = toInt32 (xor a b)
  complement (MkInt32 a) = toInt32 (complement a)
  shiftL (MkInt32 a) n = toInt32 (shiftL a n)
  shiftR (MkInt32 a) n = toInt32 (shiftR a n)
  bit n = toInt32 (bit n)
  testBit (MkInt32 a) n = testBit a n
  rotate (MkInt32 a) n = let k = mod n 32; u = mod a 4294967296 in toInt32 (u * 2 ^ k + div u (2 ^ (32 - k)))
  bitSize _ = 32
  isSigned _ = True
  popCount (MkInt32 a) = popCount (if a < 0 then a + 4294967296 else a)

newtype Int64 = MkInt64 Integer

-- The number in range that an integer is the same as, counting round.
toInt64 :: Integer -> Int64
toInt64 x = MkInt64 (mod (x + 9223372036854775808) 18446744073709551616 - 9223372036854775808)

instance Eq Int64 where
  MkInt64 a == MkInt64 b = a == b

instance Ord Int64 where
  compare (MkInt64 a) (MkInt64 b) = compare a b
  MkInt64 a <= MkInt64 b = a <= b
  MkInt64 a < MkInt64 b = a < b

instance Show Int64 where
  showsPrec p (MkInt64 a) = showsPrec p a

instance Read Int64 where
  readsPrec p s = [(toInt64 x, t) | (x, t) <- readsPrec p s]

instance Num Int64 where
  MkInt64 a + MkInt64 b = toInt64 (a + b)
  MkInt64 a - MkInt64 b = toInt64 (a - b)
  MkInt64 a * MkInt64 b = toInt64 (a * b)
  negate (MkInt64 a) = toInt64 (negate a)
  abs (MkInt64 a) = toInt64 (abs a)
  signum (MkInt64 a) = MkInt64 (signum a)
  fromInteger = toInt64

instance Real Int64 where
  toRational (MkInt64 a) = toRational a

instance Enum Int64 where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toInt64 (toInteger n)
  fromEnum (MkInt64 a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkInt64 a) (MkInt64 b) = map MkInt64 (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkInt64 a) (MkInt64 b) (MkInt64 d) = map MkInt64 (enumFromThenTo a b d)

instance Integral Int64 where
  quot (MkInt64 a) (MkInt64 b) = toInt64 (quot a b)
  rem (MkInt64 a) (MkInt64 b) = toInt64 (rem a b)
  div (MkInt64 a) (MkInt64 b) = toInt64 (div a b)
  mod (MkInt64 a) (MkInt64 b) = toInt64 (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkInt64 a) = a

instance Bounded Int64 where
  minBound = MkInt64 (negate 9223372036854775808)
  maxBound = MkInt64 9223372036854775807

instance Ix Int64 where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromInteger (toInteger i - toInteger m) else error "Ix.index: index out of range"
  inRange (m, n) i = m <= i && i <= n

instance Bits Int64 where
  MkInt64 a .&. MkInt64 b = toInt64 (a .&. b)
  MkInt64 a .|. MkInt64 b = toInt64 (a .|. b)
  xor (MkInt64 a) (MkInt64 b) = toInt64 (xor a b)
  complement (MkInt64 a) = toInt64 (complement a)
  shiftL (MkInt64 a) n = toInt64 (shiftL a n)
  shiftR (MkInt64 a) n = toInt64 (shiftR a n)
  bit n = toInt64 (bit n)
  testBit (MkInt64 a) n = testBit a n
  rotate (MkInt64 a) n = let k = mod n 64; u = mod a 18446744073709551616 in toInt64 (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = True
  popCount (MkInt64 a) = popCount (if a < 0 then a + 18446744073709551616 else a)
